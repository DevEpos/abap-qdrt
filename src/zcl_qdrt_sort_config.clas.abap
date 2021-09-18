"! <p class="shorttext synchronized" lang="en">Sort configuration</p>
CLASS zcl_qdrt_sort_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_sort_config.

    METHODS:
      constructor
        IMPORTING
          sort_fields TYPE zif_qdrt_sort_config=>ty_sort_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      sort_fields TYPE zif_qdrt_sort_config=>ty_sort_fields.
ENDCLASS.



CLASS zcl_qdrt_sort_config IMPLEMENTATION.


  METHOD constructor.
    me->sort_fields = sort_fields.
  ENDMETHOD.


  METHOD zif_qdrt_sort_config~add_sort_field.
    CHECK NOT line_exists( sort_fields[ field_name = field_name ] ).

    sort_fields = VALUE #( BASE sort_fields ( field_name = field_name sort_direction = sort_direction ) ).
  ENDMETHOD.


  METHOD zif_qdrt_sort_config~get_order_by_clause.
    FIELD-SYMBOLS:
      <sort_line> TYPE string.

    LOOP AT sort_fields ASSIGNING FIELD-SYMBOL(<sort_field>).
      IF <sort_line> IS ASSIGNED.
        <sort_line> = |{ <sort_line> },|.
      ENDIF.

      APPEND INITIAL LINE TO result ASSIGNING <sort_line>.
      <sort_line> = |{ <sort_field>-field_name } { <sort_field>-sort_direction }|.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_qdrt_sort_config~is_empty.
    result = xsdbool( sort_fields IS INITIAL ).
  ENDMETHOD.

ENDCLASS.
