import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';

// Called by the fully migrated isolated backend runner. No payment-provider
// endpoint is invoked: this verifies numeric rejection and cart write rollback.
export async function verifyCheckoutMoneyHttp({ sql, base }) {
  const seed = (prices, quantity = 1) => {
    const cart = randomUUID();
    sql(`INSERT INTO marketplace_cart(id,created_at,updated_at) VALUES ('${cart}',now(),now());`);
    const listings = prices.map(price => {
      const asset = randomUUID(), listing = randomUUID();
      sql(`INSERT INTO asset(id,name,category,condition,status,owner,maintenance_policy)
        VALUES ('${asset}','Synthetic arithmetic asset','audio','Good','Active','TDF','None');
        INSERT INTO marketplace_listing(id,asset_id,title,purpose,price_usd_cents,markup_pct,currency,active,created_at,updated_at)
        VALUES ('${listing}','${asset}','Synthetic arithmetic listing','sale',${price},25,'USD',true,now(),now());
        INSERT INTO marketplace_cart_item(cart_id,listing_id,quantity) VALUES ('${cart}','${listing}',${quantity});`);
      return listing;
    });
    return { cart, listings };
  };
  for (const [label, prices, quantity] of [
    ['aggregate overflow', ['9223372036854775807','9223372036854775807','3'], 1],
    ['product overflow', ['6148914691236517206'], 3],
    ['zero stored quantity', ['1'], 0],
    ['negative stored quantity', ['1'], -1],
    ['negative stored price', ['-1'], 1],
  ]) {
    const { cart } = seed(prices, quantity);
    assert.equal((await fetch(`${base}/marketplace/cart/${cart}`)).status, 409, label);
  }
  const valid = seed(['123','456']);
  const response = await fetch(`${base}/marketplace/cart/${valid.cart}`);
  assert.equal(response.status, 200);
  assert.equal((await response.json()).mcSubtotalCents, 579);

  const { cart, listings } = seed(['9223372036854775807','1']);
  sql(`DELETE FROM marketplace_cart_item WHERE cart_id='${cart}' AND listing_id='${listings[1]}';`);
  const snapshot = () => sql(`SELECT jsonb_build_object(
    'cart',(SELECT to_jsonb(c) FROM marketplace_cart c WHERE id='${cart}'),
    'items',(SELECT jsonb_agg(to_jsonb(i) ORDER BY id) FROM marketplace_cart_item i WHERE cart_id='${cart}'))`);
  assert.equal((await fetch(`${base}/marketplace/cart/${cart}`)).status, 200, 'the pre-update cart is valid');
  const before = snapshot();
  const update = await fetch(`${base}/marketplace/cart/${cart}/items`, {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ mciuListingId: listings[1], mciuQuantity: 1 }),
  });
  assert.equal(update.status, 409, 'an overflowing cart update returns a conflict');
  assert.match(await update.text(), /Marketplace amount exceeds supported storage/);
  assert.equal(snapshot(), before, 'rejected cart mutation rolls back rows and timestamp');
  console.log('Checkout arithmetic HTTP: typed invalid-data conflicts, exact valid totals and rejected-mutation rollback passed.');
}
