"! <p class="shorttext synchronized" lang="en">Base data selector</p>
CLASS zcl_qdrt_data_selector_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES:
      zif_qdrt_data_selector.
  PROTECTED SECTION.
    DATA:
      entity_name         TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type         TYPE zif_qdrt_ty_global=>ty_entity_type,
      filter_provider     TYPE REF TO zif_qdrt_filter_provider,
      sort_config         TYPE REF TO zif_qdrt_sort_config,
      output_field_config TYPE REF TO zif_qdrt_output_field_config,
      aggregation_config  TYPE REF TO zif_qdrt_aggregation_config,
      sql_selector        TYPE REF TO zcl_qdrt_sql_selector,
      query_result        TYPE REF TO data.

    METHODS:
      constructor
        IMPORTING
          name                TYPE zif_qdrt_ty_global=>ty_entity_name
          type                TYPE zif_qdrt_ty_global=>ty_entity_type
          filter_provider     TYPE REF TO zif_qdrt_filter_provider
          output_field_config TYPE REF TO zif_qdrt_output_field_config
          sort_config         TYPE REF TO zif_qdrt_sort_config
          aggregation_config  TYPE REF TO zif_qdrt_aggregation_config,
      "! <p class="shorttext synchronized" lang="en">Retrieves the FROM clause for the SQL select</p>
      get_from_clause
        RETURNING
          VALUE(result) TYPE string_table,
      prepare_select,
      create_result_table.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_data_selector_base IMPLEMENTATION.

  METHOD zif_qdrt_data_selector~select_data.
    FIELD-SYMBOLS:
      <query_result> TYPE table.

    prepare_select( ).
    create_result_table( ).
    IF query_result IS INITIAL.
      RETURN.
    ENDIF.

    sql_selector->set_max_rows( settings-max_rows ).
    sql_selector->set_offset( settings-offset ).

    ASSIGN query_result->* TO <query_result>.
    sql_selector->select_data( IMPORTING result = <query_result> ).
  ENDMETHOD.


  METHOD constructor.
    me->entity_name = name.
    me->entity_type = type.
    me->filter_provider = filter_provider.
    me->output_field_config = output_field_config.
    me->aggregation_config = aggregation_config.
    me->sort_config = sort_config.
  ENDMETHOD.


  METHOD prepare_select.
    sql_selector = zcl_qdrt_sql_selector=>create(
      select_clause   = output_field_config->get_select_clause( )
      from_clause     = get_from_clause( )
      where_clause    = filter_provider->get_filter_string( )
      order_by_clause = sort_config->get_order_by_clause( )
      group_by_clause = aggregation_config->get_group_by_clause( )
      having_clause   = aggregation_config->get_having_clause( ) ).
  ENDMETHOD.


  METHOD get_from_clause.
    result = VALUE #( ( |{ entity_name }| ) ).
  ENDMETHOD.


  METHOD create_result_table.

  ENDMETHOD.

ENDCLASS.
