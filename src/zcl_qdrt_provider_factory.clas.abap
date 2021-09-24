"! <p class="shorttext synchronized" lang="en">Factory for creating providers</p>
CLASS zcl_qdrt_provider_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates filter converter</p>
      create_filter_converter
        RETURNING
          VALUE(result) TYPE REF TO zif_qdrt_filter_converter,
      "! <p class="shorttext synchronized" lang="en">Creates entity metadata</p>
      create_entity_metadata
        IMPORTING
          entity_name   TYPE zif_qdrt_ty_global=>ty_entity_name
          entity_type   TYPE zif_qdrt_ty_global=>ty_entity_type
        RETURNING
          VALUE(result) TYPE REF TO zif_qdrt_entity_metadata_prov
        RAISING
          zcx_qdrt_appl_error,
      "! <p class="shorttext synchronized" lang="en">Creates VH data provider</p>
      create_vh_data_provider
        IMPORTING
          value_help_name TYPE shlpname
          value_help_type TYPE zif_qdrt_ty_global=>ty_value_help_type
        RETURNING
          VALUE(result)   TYPE REF TO zif_qdrt_vh_data_provider
        RAISING
          zcx_qdrt_appl_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_provider_factory IMPLEMENTATION.


  METHOD create_filter_converter.
    result = NEW zcl_qdrt_default_fc( ).
  ENDMETHOD.


  METHOD create_entity_metadata.
    DATA: metadata_provider TYPE REF TO zcl_qdrt_entity_metadata_base.

    metadata_provider = SWITCH #( entity_type

      WHEN zif_qdrt_c_entity_types=>cds_view THEN
        NEW zcl_qdrt_cds_emp(
          entity_type = entity_type
          entity_name = entity_name )

      WHEN zif_qdrt_c_entity_types=>database_table OR
           zif_qdrt_c_entity_types=>view THEN
        NEW zcl_qdrt_table_emp(
          entity_type = entity_type
          entity_name = entity_name ) ).

    IF metadata_provider IS NOT INITIAL.
      IF metadata_provider->zif_qdrt_entity_metadata_prov~entity_exists( ) = abap_false.
        RAISE EXCEPTION TYPE zcx_qdrt_appl_error
          EXPORTING
            status = cl_rest_status_code=>gc_client_error_not_found
            text   = |Entity with type '{ entity_type }' and name '{ entity_name }' does not exist|.
      ENDIF.
      metadata_provider->init( ).
      result = metadata_provider.
    ELSE.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          status = cl_rest_status_code=>gc_client_error_not_found
          text   = |No metadata provider found for type '{ entity_type }'|.
    ENDIF.
  ENDMETHOD.


  METHOD create_vh_data_provider.
    result = SWITCH #( value_help_type

      WHEN zif_qdrt_c_value_help_type=>fix_values THEN
        NEW zcl_qdrt_dom_fix_values_vhdp( rollname = value_help_name )

      WHEN zif_qdrt_c_value_help_type=>elementary_ddic_sh THEN
        NEW zcl_qdrt_simple_shlp_vhdp( value_help_name = value_help_name ) ).

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          text = |Value Help Type { value_help_type } is not supported|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
