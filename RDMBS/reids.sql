DROP TABLE IF EXISTS charges;
CREATE TABLE charges (
    id INTEGER PRIMARY KEY,
    date STRING,
    subtotal NUMERIC,
    taxes NUMERIC,
    total NUMERIC,
    payment_id INTEGER
);

DROP TABLE IF EXISTS items;
CREATE TABLE items (
    id INTEGER PRIMARY KEY,
    name STRING,
    price NUMERIC,
    charge_id INTEGER
);

DROP TABLE IF EXISTS payments;
CREATE TABLE payments (
    id INTEGER PRIMARY KEY,
    method STRING,
    card_type STRING,
    cardholder STRING,
    last_4_card_number STRING,
    zip STRING
);