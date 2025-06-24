CLASS zcl_generate_demo_data_582 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_generate_demo_data_582 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "delete existing entries in the database table
    DELETE FROM zrap_atrav_582.
    DELETE FROM zrap_abook_582.

    "insert travel demo data
    INSERT zrap_atrav_582 FROM (
        SELECT
            FROM /dmo/travel
            FIELDS
            uuid( ) AS travel_uuid,
            travel_id AS travel_id,
            agency_id AS agency_id,
            customer_id AS customer_id,
            begin_date AS begin_date,
            end_date AS end_date,
            booking_fee AS booking_fee,
            total_price AS total_price,
            currency_code AS currency_code,
            description AS description,
            CASE status
                WHEN 'B' THEN 'A' "accepted
                WHEN 'X' THEN 'X' "cancelled
                ELSE 'O'          "open
            END AS overall_status,
            createdby AS created_by,
            createdat AS created_at,
            lastchangedby AS last_changed_by,
            lastchangedat AS last_changed_at,
            lastchangedat AS local_last_changed_at
            ORDER BY travel_id UP TO 200 ROWS
    ).
    COMMIT WORK.

    "insert booking demo data
    INSERT zrap_abook_582 FROM (
        SELECT
            FROM /dmo/booking as booking
                JOIN zrap_atrav_582 as z
                ON booking~travel_id = z~travel_id
            FIELDS
            uuid( ) as booking_uuid,
            z~travel_uuid as travel_uuid,
            booking~booking_id as booking_id,
            booking~booking_date as booking_date,
            booking~customer_id as customer_id,
            booking~carrier_id as carrier_id,
            booking~connection_id as connection_id,
            booking~flight_date as flight_date,
            booking~flight_price as flight_price,
            booking~currency_code as currency_code,
            z~created_by as created_by,
            z~last_changed_by as last_changed_by,
            z~last_changed_at as local_last_changed_by "review
        ).
        COMMIT WORK.

    out->write( 'Travel and booking demo data inserted.' ).

  ENDMETHOD.

ENDCLASS.
