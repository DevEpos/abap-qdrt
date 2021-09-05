"! <p class="shorttext synchronized" lang="en">Output field configuration</p>
CLASS zcl_qdrt_output_field_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_output_field_config.

    METHODS:
      constructor
        IMPORTING
          field_config TYPE zif_qdrt_output_field_config=>ty_output_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_select_all TYPE string VALUE '*'.

    DATA:
      field_config TYPE zif_qdrt_output_field_config=>ty_output_fields.
ENDCLASS.



CLASS zcl_qdrt_output_field_config IMPLEMENTATION.


  METHOD constructor.
    me->field_config = field_config.
  ENDMETHOD.


  METHOD zif_qdrt_output_field_config~get_select_clause.
    IF field_config IS INITIAL.
      result = VALUE #( ( c_select_all ) ).
    ELSE.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
