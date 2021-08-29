"! <p class="shorttext synchronized" lang="en">Message Helper</p>
CLASS zcl_qdrt_message_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Fills sy-msg variables from text</p>
      set_msg_vars_from_text
        IMPORTING
          text TYPE string,
      "! <p class="shorttext synchronized" lang="en">Prints exception message</p>
      print_exc_message
        IMPORTING
          textid          TYPE scx_t100key
          print_to_screen TYPE abap_bool DEFAULT abap_true
          previous_exc    TYPE REF TO cx_root
          display_type    TYPE syst_msgty DEFAULT 'E'
          message_type    TYPE syst_msgty DEFAULT 'E'
          exc_message     TYPE REF TO zif_qdrt_exception_message
          msgv1           TYPE sy-msgv1 OPTIONAL
          msgv2           TYPE sy-msgv2 OPTIONAL
          msgv3           TYPE sy-msgv3 OPTIONAL
          msgv4           TYPE sy-msgv4 OPTIONAL
        RETURNING
          VALUE(message)   TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_message TYPE c LENGTH 200.

    CONSTANTS:
      c_length_of_msgv           TYPE i VALUE 50,
      c_offset_of_last_character TYPE i VALUE 49.

    CLASS-METHODS:
      split_text
        IMPORTING
          text         TYPE string
        EXPORTING
          VALUE(msgv1) TYPE sy-msgv1
          VALUE(msgv2) TYPE sy-msgv2
          VALUE(msgv3) TYPE sy-msgv3
          VALUE(msgv4) TYPE sy-msgv4.
ENDCLASS.



CLASS zcl_qdrt_message_helper IMPLEMENTATION.


  METHOD set_msg_vars_from_text.

    split_text(
      EXPORTING text  = text
      IMPORTING msgv1 = DATA(msgv1)
                msgv2 = DATA(msgv2)
                msgv3 = DATA(msgv3)
                msgv4 = DATA(msgv4) ).

    MESSAGE e000(zqdrt_error) WITH msgv1 msgv2 msgv3 msgv4
      INTO DATA(dummy) ##needed.
  ENDMETHOD.


  METHOD split_text.
    DATA: tmp_text TYPE ty_message,
          msg_var  TYPE c LENGTH c_length_of_msgv,
          rest     TYPE ty_message,
          index    TYPE syst-index.

    tmp_text = text.

    DO 4 TIMES.

      index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = c_length_of_msgv
          text   = tmp_text
        IMPORTING
          line   = msg_var
          rest   = rest.

      IF msg_var+c_offset_of_last_character = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        rest = | { rest }|.
      ENDIF.

      tmp_text = rest.

      CASE index.
        WHEN 1.
          msgv1 = msg_var.
        WHEN 2.
          msgv2 = msg_var.
        WHEN 3.
          msgv3 = msg_var.
        WHEN 4.
          msgv4 = msg_var.
      ENDCASE.

    ENDDO.

  ENDMETHOD.


  METHOD print_exc_message.
    IF textid-msgid = 'SY' AND textid-msgno = 530.

      " try to print message of previous exception
      IF previous_exc IS BOUND.
        TRY.
            DATA(previous_exc_msg) = CAST zif_sat_exception_message( previous_exc ).
            IF print_to_screen = abap_true.
              message = previous_exc_msg->print(
                iv_msg_type     = message_type
                iv_display_type = display_type
                if_to_screen    = print_to_screen ).
            ELSE.
              message = previous_exc_msg->get_message( ).
            ENDIF.
          CATCH cx_sy_move_cast_error.
            " Return message from non db browser exception
            IF print_to_screen = abap_true.
              MESSAGE previous_exc->get_text( ) TYPE message_type DISPLAY LIKE display_type.
            ELSE.
              message = previous_exc->get_text( ).
            ENDIF.
        ENDTRY.
      ENDIF.
    ELSE.

      IF print_to_screen = abap_true.
        MESSAGE ID textid-msgid
                TYPE   message_type
                NUMBER textid-msgno
                WITH   msgv1
                       msgv2
                       msgv3
                       msgv4
                DISPLAY LIKE display_type.
      ELSE.
        MESSAGE ID textid-msgid
                TYPE   'E'
                NUMBER textid-msgno
                WITH   msgv1
                       msgv2
                       msgv3
                       msgv4
                INTO message.
      ENDIF.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
