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
          VALUE(result) TYPE REF TO zif_qdrt_entity_metadata_prov.
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
      metadata_provider->init( ).
      result = metadata_provider.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
