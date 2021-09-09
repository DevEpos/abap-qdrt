"! <p class="shorttext synchronized" lang="en">Metadata of CDS view</p>
CLASS zcl_qdrt_cds_emp DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_qdrt_entity_metadata_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_qdrt_entity_metadata_prov~get_field_config REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_metadata REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_field_metadata REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      read_fields_metadata REDEFINITION.
  PRIVATE SECTION.
    DATA:
      header_info        TYPE dd02bv,
      ddl_view_name      TYPE viewname,
      fields_cds         TYPE dd03ndvtab,
      fields             TYPE STANDARD TABLE OF dfies WITH EMPTY KEY,
      association_header TYPE dd08bvtab,
      association_fields TYPE dd05bvtab,
      parameters         TYPE dd10bvtab,

      BEGIN OF metadata,
        fields     TYPE zif_qdrt_ty_global=>ty_fields_metadata,
        parameters TYPE zif_qdrt_ty_global=>ty_fields_metadata,
      END OF metadata.

ENDCLASS.



CLASS zcl_qdrt_cds_emp IMPLEMENTATION.


  METHOD zif_qdrt_entity_metadata_prov~get_metadata.
    result = REF #( metadata ).
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_config.
    RETURN.
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_metadata.
    CHECK metadata IS NOT INITIAL.

    IF type = zif_qdrt_c_global=>c_field_types-normal_field.
      result = VALUE #( metadata-fields[ name = to_lower( fieldname ) ] OPTIONAL ).
    ELSEIF type = zif_qdrt_c_global=>c_field_types-parameter.
      result = VALUE #( metadata-parameters[ name = to_lower( fieldname ) ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD read_fields_metadata.
    DATA(dd_sobject) = cl_dd_sobject_factory=>create( ).

    TRY.
        dd_sobject->read(
          EXPORTING
            get_state    = 'A'
            withtext     = abap_true
            sobjnames    = VALUE #( ( entity_name ) )
          IMPORTING
            dd02bv_tab   = DATA(headers)
            dd02bndv_tab = DATA(nodes)
            dd03ndv_tab  = DATA(fields_cds)
            dd08bv_tab   = association_header
            dd05bv_tab   = association_fields
            dd10bv_tab   = parameters ).

        IF headers IS INITIAL.
          RETURN.
        ENDIF.

      CATCH cx_dd_sobject_get.
        RETURN.
    ENDTRY.

    header_info = headers[ 1 ].
    ddl_view_name = nodes[ 1 ]-dbtabname.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = entity_name
      TABLES
        dfies_tab      = fields
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
      metadata-fields = VALUE #( BASE metadata-fields
        ( to_field_metadata( field_info = CORRESPONDING zif_qdrt_ty_global=>ty_field_info(
            <field> MAPPING name               = fieldname
                            is_key             = keyflag
                            length             = leng
                            has_fix_values     = valexi
                            short_description  = scrtext_s
                            medium_description = scrtext_m
                            long_description   = scrtext_l
                            field_text         = fieldtext
                            has_value_help     = f4availabl
                            ref_field          = reffield
                            ref_table          = reftable
                            is_lowercase       = lowercase ) ) ) ).
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
