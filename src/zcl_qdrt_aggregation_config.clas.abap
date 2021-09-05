"! <p class="shorttext synchronized" lang="en">Aggregation field configuration</p>
CLASS zcl_qdrt_aggregation_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_aggregation_config.

    METHODS:
      constructor
        IMPORTING
          aggregation_fields TYPE zif_qdrt_aggregation_config=>ty_aggregation_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      aggregation_fields TYPE zif_qdrt_aggregation_config=>ty_aggregation_fields.
ENDCLASS.



CLASS zcl_qdrt_aggregation_config IMPLEMENTATION.


  METHOD constructor.
    me->aggregation_fields = aggregation_fields.
  ENDMETHOD.


  METHOD zif_qdrt_aggregation_config~get_having_clause.

  ENDMETHOD.


  METHOD zif_qdrt_aggregation_config~get_order_by_clause.

  ENDMETHOD.


ENDCLASS.
