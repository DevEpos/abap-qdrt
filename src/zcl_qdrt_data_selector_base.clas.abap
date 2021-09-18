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
      metadata_provider   TYPE REF TO zif_qdrt_entity_metadata_prov,
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
          metadata_provider   TYPE REF TO zif_qdrt_entity_metadata_prov
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
    METHODS fix_sort_config.
ENDCLASS.



CLASS zcl_qdrt_data_selector_base IMPLEMENTATION.

  METHOD zif_qdrt_data_selector~select_data.
    FIELD-SYMBOLS:
      <query_result> TYPE table.

    IF settings-offset > 0.
      fix_sort_config( ).
    ENDIF.
    prepare_select( ).
    create_result_table( ).
    IF query_result IS INITIAL.
      RETURN.
    ENDIF.

    sql_selector->set_max_rows( settings-max_rows ).
    sql_selector->set_offset( settings-offset ).

    ASSIGN query_result->* TO <query_result>.
    sql_selector->select_data( IMPORTING result = <query_result> ).

    result = query_result.
  ENDMETHOD.


  METHOD zif_qdrt_data_selector~get_max_count.
    DATA:
      count_result TYPE REF TO data.

    prepare_select( ).

    IF aggregation_config->is_empty( ) = abap_false.
      create_result_table( ).
      CREATE DATA count_result LIKE query_result.
      result = sql_selector->determine_size_for_group_by( count_result ).
    ELSE.
      result = sql_selector->determine_size( ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    me->entity_name = name.
    me->entity_type = type.
    me->metadata_provider = metadata_provider.
    me->filter_provider = filter_provider.
    me->output_field_config = output_field_config.
    me->aggregation_config = aggregation_config.
    me->sort_config = sort_config.
  ENDMETHOD.


  METHOD prepare_select.
    CHECK sql_selector IS INITIAL.

    output_field_config->set_use_count_field( xsdbool( aggregation_config->is_empty( ) = abap_false ) ).

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
    FIELD-SYMBOLS: <lt_table>      TYPE STANDARD TABLE,
                   <lt_table_temp> TYPE STANDARD TABLE.
    DATA: tab_components TYPE abap_component_tab .

    CHECK query_result IS INITIAL.

    LOOP AT metadata_provider->get_fields_metadata( ) ASSIGNING FIELD-SYMBOL(<field_metadata>).
      CHECK output_field_config->is_output_field( CONV #( <field_metadata>-name ) ).

      tab_components = VALUE #( BASE tab_components
        ( name = to_upper( <field_metadata>-name )
          type = CAST #( cl_abap_typedescr=>describe_by_name(
            COND string( WHEN <field_metadata>-rollname IS NOT INITIAL THEN <field_metadata>-rollname
                         ELSE |{ entity_name }-{ <field_metadata>-name }| ) ) ) ) ).
    ENDLOOP.

    IF aggregation_config->is_empty( ) = abap_false.
      tab_components = VALUE #( BASE tab_components
       ( name = zif_qdrt_c_global=>c_custom_field_names-line_index
         type = CAST #( cl_abap_typedescr=>describe_by_data( VALUE zqdrt_no_of_lines( ) ) ) ) ).
    ENDIF.

    DATA(dyn_struct_descr) = cl_abap_structdescr=>get(
      p_components = tab_components ).

    DATA(dyn_table_descr) = cl_abap_tabledescr=>get(
      p_line_type = dyn_struct_descr ).

    CREATE DATA query_result TYPE HANDLE dyn_table_descr.

  ENDMETHOD.


  METHOD fix_sort_config.
    CHECK sort_config->is_empty( ).

    DATA(fields_meta) = metadata_provider->get_fields_metadata( ).

    LOOP AT fields_meta ASSIGNING FIELD-SYMBOL(<field_meta>) WHERE is_key = abap_true.
      sort_config->add_sort_field( CONV #( <field_meta>-name ) ).
    ENDLOOP.

    " No key fields (possible if CDS view), add the first field instead as sort field
    IF sy-subrc <> 0.
      sort_config->add_sort_field( CONV #( fields_meta[ 1 ]-name ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
