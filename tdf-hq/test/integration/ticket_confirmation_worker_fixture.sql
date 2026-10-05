INSERT INTO social_event(id,title,timezone,start_time,created_at,updated_at)
VALUES(1,'Synthetic workshop','America/Guayaquil','2026-10-24T19:00:00Z',NOW(),NOW());
INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,created_at,updated_at)
VALUES(1,1,'general','General',2000,'USD',20,2,TRUE,NOW(),NOW());
INSERT INTO event_ticket_order(id,event_id,tier_id,buyer_name,buyer_email,quantity,amount_cents,currency,status,purchased_at,created_at,updated_at)
VALUES(1,1,1,'Synthetic buyer','buyer@example.invalid',2,4000,'USD','paid',NOW(),NOW(),NOW());
INSERT INTO event_ticket_checkout_policy(id,event_id)
VALUES('c0200000-0000-4000-8000-000000000001',1);
INSERT INTO event_ticket_checkout_runtime(order_id,policy_id,event_id,payment_status)
VALUES(1,'c0200000-0000-4000-8000-000000000001',1,'paid');
INSERT INTO event_ticket(id,event_id,tier_ref_id,order_ref_id,holder_name,holder_email,code,status,created_at,updated_at)
VALUES(1,1,1,1,'Synthetic buyer','buyer@example.invalid','TDF-ABCDEF012345','issued',NOW(),NOW()),
      (2,1,1,1,'Synthetic buyer','buyer@example.invalid','TDF-ABCDEF012346','issued',NOW(),NOW());
