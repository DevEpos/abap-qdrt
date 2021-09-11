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
    FIELD-SYMBOLS:
      <having_line> TYPE string.

    CHECK aggregation_config-having_expressions IS NOT INITIAL.

    LOOP AT aggregation_config-having_expressions ASSIGNING FIELD-SYMBOL(<having_expr>).
      IF <having_line> IS ASSIGNED.
      ENDIF.

      APPEND INITIAL LINE TO result ASSIGNING <having_line>.
      IF <having_expr>-operation = zif_qdrt_c_filter_ops=>between.
        <having_line> = |{ <having_expr>-function }( { <having_expr>-field_name } )| &&
          | BETWEEN { cl_abap_dyn_prg=>quote( <having_expr>-value1 ) } AND { cl_abap_dyn_prg=>quote( <having_expr>-value2 ) }|.
      ELSE.
        <having_line> = |{ <having_expr>-function }( { <having_expr>-field_name } ) { <having_expr>-operation }| &&
         | { cl_abap_dyn_prg=>quote( <having_expr>-value1 ) }|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_qdrt_aggregation_config~get_group_by_clause.
    FIELD-SYMBOLS:
      <group_by_line> TYPE string.

    CHECK aggregation_config-aggregation_expressions IS NOT INITIAL.

    LOOP AT aggregation_config-aggregation_expressions ASSIGNING FIELD-SYMBOL(<group_by>).
      IF <group_by_line> IS ASSIGNED.
        <group_by_line> = |{ <group_by_line> }, |.
      ENDIF.

      APPEND INITIAL LINE TO result ASSIGNING <group_by_line>.
      <group_by_line> = <group_by>-field_name.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_qdrt_aggregation_config~is_empty.
    result = xsdbool( aggregation_config-aggregation_expressions IS INITIAL ).
  ENDMETHOD.

  METHOD zif_qdrt_aggregation_config~is_field_in_aggr_expr.
    result = xsdbool( line_exists( aggregation_config-aggregation_expressions[ field_name = fieldname ] ) ).
  ENDMETHOD.

  METHOD zif_qdrt_aggregation_config~is_field_in_having_expr.
    result = xsdbool( line_exists( aggregation_config-having_expressions[ field_name = fieldname ] ) ).
  ENDMETHOD.

ENDCLASS.
