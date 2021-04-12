*&---------------------------------------------------------------------*
*& Report zflight_evaluation_input_150
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zflight_evaluation_input_150.

CLASS lcl_flight_evaluation DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS co_carrid_lh TYPE s_carrid VALUE 'LH'.
    METHODS constructor
      IMPORTING
        i_connid TYPE s_conn_id
        i_fldate TYPE s_date.
    METHODS run.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA connid TYPE s_conn_id.
    DATA fldate TYPE s_date.
    DATA it_evaluation TYPE zif_flight_evaluation_150=>ty_evaluation_tab.
    DATA s_alv_table   TYPE REF TO cl_salv_table.

    METHODS on_double_click FOR EVENT double_click
                OF cl_salv_events_table
      IMPORTING row column.

ENDCLASS.

CLASS lcl_flight_evaluation IMPLEMENTATION.

  METHOD constructor.
    me->connid = i_connid.
    me->fldate = i_fldate.
  ENDMETHOD.


  METHOD run.

    zcl_flight_evaluation_150=>get_evaluations_by_flight_data(
      EXPORTING
        i_carrid      = co_carrid_lh
        i_connid      = me->connid
        i_fldate      = me->fldate
      RECEIVING
        it_evaluation = it_evaluation
    ).

    IF it_evaluation IS INITIAL.
      zcl_flight_evaluation_150=>create_flight_evaluation(
        EXPORTING
          i_carrid = co_carrid_lh
          i_connid = me->connid
          i_fldate = me->fldate
      ).
      zcl_flight_evaluation_150=>get_evaluations_by_flight_data(
       EXPORTING
         i_carrid      = co_carrid_lh
         i_connid      = me->connid
         i_fldate      = me->fldate
       RECEIVING
         it_evaluation = it_evaluation ).
    ENDIF.

    DATA: alv_events TYPE REF TO cl_salv_events_table.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = s_alv_table
      CHANGING
        t_table        = it_evaluation
    ).

    s_alv_table->get_columns( )->set_optimize(
*        value = IF_SALV_C_BOOL_SAP~TRUE
    ).

    alv_events = s_alv_table->get_event( ).
    SET HANDLER on_double_click FOR alv_events.

    s_alv_table->display( ).
  ENDMETHOD.

  METHOD on_double_click.
    DATA: lt_fields     TYPE STANDARD TABLE OF sval,
          ls_fields     TYPE sval,
          lv_returncode TYPE c.

    DATA evaluation TYPE zif_flight_evaluation_150=>ty_evaluation.

    READ TABLE it_evaluation INTO evaluation INDEX row.



    ls_fields-tabname   = 'ZFLIGHT_EVAL'.
    ls_fields-fieldname = 'MEAL_RATING'.
    ls_fields-field_obl = ' '.
    ls_fields-value = evaluation-meal_rating.
    APPEND ls_fields TO lt_fields.
    CLEAR ls_fields.

    ls_fields-tabname   = 'ZFLIGHT_EVAL'.
    ls_fields-fieldname = 'FLIGHT_RATING'.
    ls_fields-field_obl = ' '.
    ls_fields-value = evaluation-flight_rating.
    APPEND ls_fields TO lt_fields.
    CLEAR ls_fields.

    ls_fields-tabname   = 'ZFLIGHT_EVAL'.
    ls_fields-fieldname = 'SERVICE_RATING'.
    ls_fields-field_obl = ' '.
    ls_fields-value = evaluation-service_rating.
    APPEND ls_fields TO lt_fields.
    CLEAR ls_fields.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
*       NO_VALUE_CHECK  = ' '
        popup_title     = 'Enter Evaluation of Passenger'
        start_column    = '5'
        start_row       = '5'
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE i531(0u) WITH 'Problem with Fuba POPUP_GET_VALUES'.
    ENDIF.

    IF lv_returncode = 'A'.
      EXIT.
    ENDIF.

    " Meal Rating
    READ TABLE lt_fields INTO ls_fields INDEX 1.
    evaluation-meal_rating = ls_fields-value.
    " Flight Rating
    READ TABLE lt_fields INTO ls_fields INDEX 2.
    evaluation-flight_rating = ls_fields-value.
    "Service Rating
    READ TABLE lt_fields INTO ls_fields INDEX 3.
    evaluation-service_rating = ls_fields-value.

    DATA eval_obj TYPE REF TO zif_flight_evaluation_150.
    eval_obj = NEW zcl_flight_evaluation_150(
        i_carrid = co_carrid_lh
        i_connid = connid
        i_fldate = fldate
        i_bookid = evaluation-bookid
    ).

    eval_obj->set_customer_id( evaluation-customid ).
    eval_obj->set_customer_name( evaluation-name ).
    eval_obj->set_flight_rating( evaluation-flight_rating ).
    eval_obj->set_meal_rating( evaluation-meal_rating ).
    eval_obj->set_service_rating( evaluation-service_rating ).

    eval_obj->save_on_db( ).

    FIELD-SYMBOLS: <evaluation> LIKE LINE OF it_evaluation.
    READ TABLE it_evaluation ASSIGNING <evaluation> INDEX row.
    <evaluation>-meal_rating = evaluation-meal_rating.
    <evaluation>-flight_rating = evaluation-flight_rating.
    <evaluation>-service_rating = evaluation-service_rating.

    s_alv_table->refresh( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  PARAMETERS connid TYPE s_conn_id.
  PARAMETERS fldate TYPE s_date.

  DATA runner TYPE REF TO lcl_flight_evaluation.

  runner = NEW lcl_flight_evaluation(
      i_connid = connid
      i_fldate = fldate
  ).

  runner->run( ).
