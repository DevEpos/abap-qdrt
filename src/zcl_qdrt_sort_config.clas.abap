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


  METHOD zif_qdrt_sort_config~get_order_by_clause.

  ENDMETHOD.


ENDCLASS.
