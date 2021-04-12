*&---------------------------------------------------------------------*
*& Report  zflight_customer_satisfaction
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zflight_customer_satisfact_150.



DATA(alv) = cl_salv_gui_table_ida=>create_for_cds_view( 'ZFLIGHT_EVALUATION_VIEW_150' ).

DATA sort_order TYPE if_salv_gui_types_ida=>yt_sort_rule.

INSERT VALUE #(  field_name = 'CUSTOMER_NAME ' descending = abap_false is_grouped = abap_true ) INTO TABLE sort_order.

alv->default_layout( )->set_sort_order( it_sort_order = sort_order  ).

alv->fullscreen( )->display( ).
