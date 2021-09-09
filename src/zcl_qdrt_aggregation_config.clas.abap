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
          aggregation_config TYPE zif_qdrt_aggregation_config=>ty_aggregation_config.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      aggregation_config TYPE zif_qdrt_aggregation_config=>ty_aggregation_config.
ENDCLASS.



CLASS zcl_qdrt_aggregation_config IMPLEMENTATION.


  METHOD constructor.
    me->aggregation_config = aggregation_config.
  ENDMETHOD.


  METHOD zif_qdrt_aggregation_config~get_having_clause.
    CHECK aggregation_config-having_expressions IS NOT INITIAL.
  ENDMETHOD.


  METHOD zif_qdrt_aggregation_config~get_group_by_clause.
    CHECK aggregation_config-aggregation_expressions IS NOT INITIAL.

  ENDMETHOD.


  METHOD zif_qdrt_aggregation_config~is_empty.
    result = xsdbool( aggregation_config-aggregation_expressions IS INITIAL ).
  ENDMETHOD.

  METHOD zif_qdrt_aggregation_config~is_field_in_aggr_expr.
    result = xsdbool( line_exists( aggregation_config-aggregation_expressions[ column_key = fieldname ] ) ).
  ENDMETHOD.

  METHOD zif_qdrt_aggregation_config~is_field_in_having_expr.
    result = xsdbool( line_exists( aggregation_config-having_expressions[ column_key = fieldname ] ) ).
  ENDMETHOD.

ENDCLASS.
