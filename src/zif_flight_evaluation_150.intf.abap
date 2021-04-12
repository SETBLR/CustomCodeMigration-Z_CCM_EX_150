INTERFACE ZIF_FLIGHT_EVALUATION_150
  PUBLIC .
  TYPES: BEGIN OF ty_evaluation,
           bookid         TYPE z_bookid,
           customid       TYPE z_customer,
           name           TYPE z_customer_name,
           meal_rating    TYPE z_meal_rating,
           flight_rating  TYPE z_flight_rating,
           service_rating TYPE z_service_rating,
         END OF ty_evaluation,
         ty_evaluation_tab TYPE STANDARD TABLE OF ty_evaluation WITH KEY bookid.
  METHODS set_meal_rating
    IMPORTING
      i_meal_rating TYPE z_meal_rating OPTIONAL.
  METHODS set_flight_rating
    IMPORTING
      i_flight_rating TYPE z_flight_rating OPTIONAL.
  METHODS set_customer_id
    IMPORTING
      i_customer_id TYPE z_customer OPTIONAL.
  METHODS set_service_rating
    IMPORTING
      i_service_rating TYPE z_service_rating OPTIONAL.
  METHODS save_on_db.
  METHODS evaluation_exist
    RETURNING
      VALUE(r_evaluation_exist) TYPE boole_d.
  METHODS set_customer_name
    IMPORTING
      i_customer_name TYPE z_customer_name OPTIONAL.

ENDINTERFACE.
