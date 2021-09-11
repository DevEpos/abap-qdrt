"! <p class="shorttext synchronized" lang="en">Exception for application errors</p>
CLASS zcx_qdrt_appl_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      if_t100_message,
      zif_qdrt_exception_message.

    ALIASES:
      get_message FOR zif_qdrt_exception_message~get_message.

    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZQDRT_ERROR',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error.

    DATA:
      msgv1 TYPE sy-msgv1,
      msgv2 TYPE sy-msgv2,
      msgv3 TYPE sy-msgv3,
      msgv4 TYPE sy-msgv4.

    METHODS:
      constructor
        IMPORTING
          text     TYPE string OPTIONAL
          textid   LIKE if_t100_message=>t100key OPTIONAL
          previous LIKE previous OPTIONAL
          msgv1    TYPE sy-msgv1 OPTIONAL
          msgv2    TYPE sy-msgv2 OPTIONAL
          msgv3    TYPE sy-msgv3 OPTIONAL
          msgv4    TYPE sy-msgv4 OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_qdrt_appl_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA: fill_t100_from_sy TYPE abap_bool.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.

    IF text IS NOT INITIAL.
      fill_t100_from_sy = abap_true.
      zcl_qdrt_message_helper=>set_msg_vars_from_text( text ).
    ELSEIF sy-msgid IS NOT INITIAL.
      fill_t100_from_sy = abap_true.
    ELSEIF textid IS NOT INITIAL.
      if_t100_message~t100key = textid.
      me->msgv1 = msgv1.
      me->msgv2 = msgv2.
      me->msgv3 = msgv3.
      me->msgv4 = msgv4.
    ELSE.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ENDIF.

    IF fill_t100_from_sy = abap_true.
      me->msgv1 = sy-msgv1.
      me->msgv2 = sy-msgv2.
      me->msgv3 = sy-msgv3.
      me->msgv4 = sy-msgv4.
      if_t100_message~t100key = VALUE #(
        msgid = sy-msgid
        msgno = sy-msgno
        attr1 = 'MSGV1'
        attr2 = 'MSGV2'
        attr3 = 'MSGV3'
        attr4 = 'MSGV4' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_qdrt_exception_message~get_message.
    result = zcl_qdrt_message_helper=>print_exc_message(
      textid          = if_t100_message~t100key
      print_to_screen = abap_false
      previous_exc    = previous
      exc_message     = me
      msgv1           = msgv1
      msgv2           = msgv2
      msgv3           = msgv3
      msgv4           = msgv4 ).
  ENDMETHOD.


  METHOD zif_qdrt_exception_message~print.
    zcl_qdrt_message_helper=>print_exc_message(
      textid          = if_t100_message~t100key
      display_type    = iv_display_type
      print_to_screen = if_to_screen
      message_type    = iv_msg_type
      previous_exc    = previous
      exc_message     = me
      msgv1           = msgv1
      msgv2           = msgv2
      msgv3           = msgv3
      msgv4           = msgv4 ).
  ENDMETHOD.


ENDCLASS.
