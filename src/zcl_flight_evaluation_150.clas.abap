CLASS ZCL_FLIGHT_EVALUATION_150 DEFINITION
  PUBLIC

  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_flight_evaluation_150.

    METHODS constructor
      IMPORTING
        i_carrid TYPE z_carrid
        i_connid TYPE z_connid
        i_fldate TYPE d
        i_bookid TYPE z_bookid.

    CLASS-METHODS create_flight_evaluation
      IMPORTING
        i_carrid TYPE z_carrid
        i_connid TYPE z_connid
        i_fldate TYPE s_date.

    CLASS-METHODS get_evaluations_by_flight_data
      IMPORTING
        i_carrid             TYPE z_carrid
        i_connid             TYPE z_connid
        i_fldate             TYPE d
      RETURNING
        VALUE(it_evaluation) TYPE zif_flight_evaluation_150~ty_evaluation_tab.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA carrid TYPE z_carrid.
    DATA connid TYPE z_connid.
    DATA fldate TYPE d.
    DATA bookid TYPE z_bookid.
    DATA customer_id TYPE z_customer.
    DATA customer_name TYPE z_customer_name.
    DATA meal_rating TYPE z_meal_rating.
    DATA flight_rating TYPE z_flight_rating.
    DATA service_rating TYPE z_service_rating.
    DATA evaluation_exist_indicator TYPE boole_d.
    METHODS get_system_info.


ENDCLASS.



CLASS ZCL_FLIGHT_EVALUATION_150 IMPLEMENTATION.


  METHOD constructor.
    me->carrid = i_carrid.
    me->connid = i_connid.
    me->fldate = i_fldate.
    me->bookid = i_bookid.

    DATA wa_zflight_eval TYPE zflight_eval.
    SELECT SINGLE * FROM zflight_eval
      WHERE carrid = @i_carrid
        AND connid = @i_connid
        AND fldate = @i_fldate
        AND bookid = @i_bookid
      INTO @wa_zflight_eval  .

    IF sy-subrc = 0.
      me->flight_rating = wa_zflight_eval-flight_rating.
      me->meal_rating = wa_zflight_eval-meal_rating.
      me->service_rating = wa_zflight_eval-service_rating.
      me->evaluation_exist_indicator = 'X'.
    ELSE.
      me->evaluation_exist_indicator = ' '.
    ENDIF.

  ENDMETHOD.


  METHOD create_flight_evaluation.
    DATA it_eval TYPE STANDARD TABLE OF zflight_eval.
    DATA wa_eval TYPE zflight_eval.
    "data tmp type /BEV1/BO_CON_FLAG.
    DATA boole TYPE boole_d.

    TYPES: BEGIN OF booking_list,
             carrid     TYPE z_carrid,
             connid     TYPE z_connid,
             fldate     TYPE d,
             bookid     TYPE z_bookid,
             customid   TYPE z_customer,
             order_date TYPE d,
             cancelled  TYPE abap_bool,
           END OF booking_list.

*    select booking~carrid booking~connid booking~fldate booking~bookid booking~customid customer~name
*      from sbook as booking join scustom as customer on customer~id = booking~customid
*
*      into corresponding fields of table it_eval
*       where booking~carrid = i_carrid
*         and booking~connid = i_connid
*         and booking~fldate = i_fldate.

    DATA booking_list  TYPE STANDARD TABLE OF booking_list.
    FIELD-SYMBOLS: <booking_list_item> TYPE booking_list.
    CALL FUNCTION 'BAPI_SBOOK_GETLIST'
      EXPORTING
        airlinecarrier   = i_carrid    " Carrier ID
        connectionnumber = i_connid  " Connection number
        dateofflight     = i_fldate    " Departure date
      TABLES
        bookinglist      = booking_list.

    LOOP AT booking_list ASSIGNING <booking_list_item>.
      wa_eval-bookid = <booking_list_item>-bookid.
      wa_eval-carrid = <booking_list_item>-carrid.
      wa_eval-connid = <booking_list_item>-connid.
      wa_eval-fldate = <booking_list_item>-fldate.
      wa_eval-customid = <booking_list_item>-customid.
    ENDLOOP.

    MODIFY zflight_eval FROM TABLE @it_eval.

  ENDMETHOD.


  METHOD get_evaluations_by_flight_data.

    REFRESH it_evaluation.

    SELECT bookid, customid, name, meal_rating, flight_rating, service_rating
     FROM zflight_eval
      WHERE carrid = @i_carrid
        AND connid = @i_connid
        AND fldate = @i_fldate
     INTO TABLE @it_evaluation.

    LOOP AT it_evaluation ASSIGNING FIELD-SYMBOL(<fs>).
      DATA(first_evaluation) = <fs>.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_system_info.
    DATA uname TYPE uname.
    DATA xfeld TYPE feld.
    uname = sy-uname.

    DATA language TYPE langu.
    language = sy-langu.

  ENDMETHOD.


  METHOD zif_flight_evaluation_150~evaluation_exist.
    r_evaluation_exist = me->evaluation_exist_indicator.
  ENDMETHOD.


  METHOD zif_flight_evaluation_150~save_on_db.
    DATA wa_zflight_eval TYPE zflight_eval.

    wa_zflight_eval-bookid = me->bookid.
    wa_zflight_eval-carrid = me->carrid.
    wa_zflight_eval-connid = me->connid.
    wa_zflight_eval-fldate = me->fldate.
    wa_zflight_eval-customid = me->customer_id.
    wa_zflight_eval-name = me->customer_name.
    wa_zflight_eval-flight_rating = me->flight_rating.
    wa_zflight_eval-service_rating = me->service_rating.
    wa_zflight_eval-meal_rating = me->meal_rating.


    "data eval_exit type ref to ZFLIGHT_EVAL_BADI.
    "get badi eval_exit.

    "call badi eval_exit->change_evaluation_before_save
    "  changing
    "    evaluation = wa_zflight_eval.

    MODIFY zflight_eval FROM @wa_zflight_eval.

  ENDMETHOD.


  METHOD zif_flight_evaluation_150~set_customer_id.
    me->customer_id = i_customer_id.
  ENDMETHOD.


  METHOD zif_flight_evaluation_150~set_customer_name.
    me->customer_name = i_customer_name.
  ENDMETHOD.


  METHOD zif_flight_evaluation_150~set_flight_rating.
    me->flight_rating = i_flight_rating.
  ENDMETHOD.


  METHOD zif_flight_evaluation_150~set_meal_rating.
    "me->meal_rating = i_meal_rating.
    MOVE i_meal_rating TO me->meal_rating.
  ENDMETHOD.


  METHOD zif_flight_evaluation_150~set_service_rating.
    me->service_rating = i_service_rating.
  ENDMETHOD.
ENDCLASS.
