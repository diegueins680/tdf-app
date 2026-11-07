"""PAY-MUSIC-MONEY-WIRE-001: synthetic HTTP/SQL boundary, no provider calls."""
import json
import uuid


def verify_music_money(sql, request, check, actor, root):
    artist = actor('artist', 'money-artist')
    buyer = actor('fan', 'money-buyer')
    other = actor('fan', 'money-other')
    for ident in [buyer, other]:
        sql("UPDATE party SET primary_email='synthetic-money-" + ident + "@example.test' WHERE id=" + ident)
    release, version, asset, rule = [str(uuid.uuid4()) for _ in range(4)]
    slug = 'synthetic-money-' + release
    # Seed only a published offer; publication workflow has separate coverage.
    # Bypass publication immutability guards only during this owned fixture setup.
    sql(f"""BEGIN; SET LOCAL session_replication_role=replica;
      INSERT INTO music_release(id,artist_party_id,canonical_slug,release_kind,created_by)
        VALUES ('{release}',{artist},'{slug}','single',{artist});
      INSERT INTO music_release_version(id,release_id,version_number,state,title,display_artist,
        published_at,created_by,approved_by,approved_at,release_at_utc,release_timezone,
        immutable_snapshot,snapshot_sha256)
        VALUES ('{version}','{release}',1,'published','Synthetic exact price','Synthetic artist',
          now()-interval '1 hour',{artist},{artist},now()-interval '2 hours',
          now()-interval '1 hour','UTC','{{}}',repeat('b',64));
      UPDATE music_release SET published_version_id='{version}' WHERE id='{release}';
      INSERT INTO music_asset(id,release_version_id,asset_role,storage_provider,bucket_name,
        object_key,media_type,byte_size,sha256,processing_state,immutable,created_by,ready_at)
        VALUES ('{asset}','{version}','cover_original','s3_compatible','synthetic',
          'synthetic/no-blob','application/octet-stream',4,repeat('a',64),'ready',true,{artist},now());
      INSERT INTO music_availability_rule(id,release_version_id,listening_policy,download_policy,
        purchasable,price_minor,currency,downloadable_asset_id)
        VALUES ('{rule}','{version}','none','purchase',true,9007199254740993,'USD','{asset}');
      COMMIT;""")
    flags = json.loads(sql("SELECT json_agg(json_build_object('key',flag_key,'enabled',enabled)) "
        "FROM revenue_feature_flag WHERE environment='sandbox' AND flag_key IN "
        "('music_releases.public','music_releases.commerce')"))
    check('music fixture has exactly two sandbox controls', len(flags) == 2)
    sql("UPDATE revenue_feature_flag SET enabled=true WHERE environment='sandbox' "
        "AND flag_key IN ('music_releases.public','music_releases.commerce')")
    try:
        def exact_public():
            status, body = request('/music/releases/' + slug, token=None)
            return status == 200 and json.loads(body)['availability'][0]['priceMinor'] == '9007199254740993'

        check('music public HTTP price is exact decimal text', exact_public())
        # A controlled SQL mutation keeps HTTP healthy but must fail the exact
        # representation oracle. Restore the canonical function even on failure.
        try:
            sql('CREATE OR REPLACE FUNCTION public.music_minor_units_json(amount BIGINT) '
                'RETURNS JSONB LANGUAGE SQL IMMUTABLE STRICT PARALLEL SAFE AS $$ SELECT to_jsonb(amount) $$;')
            status, body = request('/music/releases/' + slug, token=None)
            mutated_price = json.loads(body)['availability'][0]['priceMinor']
            check('music lossy SQL mutation preserves a healthy HTTP response',
                  status == 200 and type(mutated_price) is int and mutated_price == 9007199254740993)
            check('music lossy SQL mutation fails the actual HTTP exact-price oracle',
                  mutated_price != '9007199254740993')
        finally:
            sql((root / 'tdf-hq/sql/2026-10-15_music_exact_minor_units.sql').read_text())
        check('music exact SQL encoding is restored', exact_public())
        payload = {'availabilityRuleId': rule, 'territoryCode': 'ZZ', 'termsVersion': 'synthetic-money-v1',
                   'expectedAmountMinor': '9007199254740993', 'expectedCurrency': 'USD'}
        key = 'synthetic-money-' + release
        def create(value, token='fixture-money-buyer'):
            return request('/music/purchases', value, token=token, method='POST', idempotency=key)
        check('music purchase POST denies anonymous actor', create(payload, None)[0] == 401)
        check('music purchase list denies anonymous actor', request('/music/purchases', token=None)[0] == 401)
        legacy = {k: v for k, v in payload.items() if not k.startswith('expected')}
        check('music HTTP decoder refuses missing price confirmation', create(legacy)[0] == 400)
        for amount, currency, expected in [('1', 'USD', 409), ('9007199254740993', 'EUR', 409),
                ('0', 'USD', 400), ('01', 'USD', 400), ('9223372036854775808', 'USD', 400),
                ('9007199254740993', 'usd', 400)]:
            check('music HTTP quote admission ' + amount + '/' + currency,
                  create(dict(payload, expectedAmountMinor=amount, expectedCurrency=currency))[0] == expected)
        check('music refused quotes have no purchase effects',
              sql("SELECT count(*) FROM music_purchase_order WHERE release_version_id='" + version + "'") == '0')
        status, body = create(payload)
        purchase = json.loads(body)
        check('music accepted HTTP purchase preserves exact gross and net', status == 201 and
              purchase['grossMinor'] == '9007199254740993' and purchase['netMinor'] == '9007199254740993' and
              all(purchase[field] == 0 for field in ['discountMinor', 'taxMinor', 'feeMinor']))
        status, body = create(payload)
        check('music exact HTTP replay returns the same purchase', status == 201 and json.loads(body)['id'] == purchase['id'])
        check('music changed terms conflict on HTTP replay', create(dict(payload, termsVersion='other-terms'))[0] == 409)
        status, body = request('/music/purchases', token='fixture-money-other')
        check('music HTTP list denies horizontal disclosure', status == 200 and json.loads(body) == [])
        status, body = request('/music/purchases', token='fixture-money-buyer')
        check('music owner HTTP list preserves exact amount', status == 200 and len(json.loads(body)) == 1 and
              json.loads(body)[0]['id'] == purchase['id'] and json.loads(body)[0]['grossMinor'] == '9007199254740993')
        check('music unpaid HTTP purchase creates no provider attempt',
              sql("SELECT count(*) FROM commerce_payment_attempt WHERE checkout_id='" + purchase['checkoutId'] + "'") == '0')
        check('music unpaid HTTP purchase grants no download entitlement',
              sql("SELECT count(*) FROM music_entitlement WHERE purchase_order_id='" + purchase['id'] + "'") == '0')
    finally:
        for flag in flags:
            sql("UPDATE revenue_feature_flag SET enabled=" + ('true' if flag['enabled'] else 'false') +
                " WHERE environment='sandbox' AND flag_key='" + flag['key'] + "'")
    restored = json.loads(sql("SELECT json_agg(json_build_object('key',flag_key,'enabled',enabled)) "
        "FROM revenue_feature_flag WHERE environment='sandbox' AND flag_key IN "
        "('music_releases.public','music_releases.commerce')"))
    check('music sandbox controls restored to their original values',
          sorted(restored, key=lambda row: row['key']) == sorted(flags, key=lambda row: row['key']))
