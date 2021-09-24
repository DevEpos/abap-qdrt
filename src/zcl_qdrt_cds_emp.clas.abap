"! <p class="shorttext synchronized" lang="en">Metadata of CDS view</p>
CLASS zcl_qdrt_cds_emp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_qdrt_entity_metadata_prov.
    METHODS:
      constructor
        IMPORTING
          cds_name TYPE zif_qdrt_ty_global=>ty_entity_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      header_info        TYPE dd02bv,
      ddl_view_name      TYPE viewname,
      fields_cds         TYPE dd03ndvtab,
      association_header TYPE dd08bvtab,
      association_fields TYPE dd05bvtab,
      parameters         TYPE dd10bvtab,
      entity_name        TYPE zif_qdrt_ty_global=>ty_entity_name,
      exists             TYPE abap_bool VALUE abap_undefined,

      BEGIN OF metadata,
        BEGIN OF entity,
          name        TYPE string,
          raw_name    TYPE string,
          description TYPE string,
        END OF entity,
        fields     TYPE zif_qdrt_ty_global=>ty_fields_metadata,
        parameters TYPE zif_qdrt_ty_global=>ty_fields_metadata,
      END OF metadata.

    METHODS:
      fill_fields_metadata
        IMPORTING
          fields_cds TYPE dd03ndvtab,
      fill_param_metadata
        IMPORTING
          params TYPE dd10bvtab,
      read_metadata.
ENDCLASS.



CLASS zcl_qdrt_cds_emp IMPLEMENTATION.


  METHOD constructor.
    me->entity_name = cds_name.
  ENDMETHOD.


  METHOD zif_qdrt_metadata_provider~entity_exists.

    IF exists = abap_undefined.
      IF sy-saprl >= 751.
        DATA(source_type_where_cond) = `source_type IN @<valid_source_types>`.
        ASSIGN zcl_qdrt_cds_util=>valid_ddl_source_types TO FIELD-SYMBOL(<valid_source_types>).

        SELECT SINGLE @abap_true
          FROM ddddlsrc AS ddl
            INNER JOIN zqdrt_i_cdsview AS cds
              ON ddl~ddlname = cds~altentityid
          WHERE entityid = @entity_name
            AND (source_type_where_cond)
          INTO @exists.
      ELSE.
        SELECT SINGLE @abap_true
          FROM zqdrt_i_cdsview
          WHERE entityid = @entity_name
          INTO @exists.
      ENDIF.
      IF sy-subrc <> 0.
        exists = abap_false.
      ENDIF.
    ENDIF.
    result = exists.
  ENDMETHOD.


  METHOD zif_qdrt_metadata_provider~get_metadata.
    read_metadata( ).
    result = REF #( metadata ).
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_config.
    RETURN.
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_metadata.
    read_metadata( ).
    CHECK metadata IS NOT INITIAL.

    IF type = zif_qdrt_c_global=>c_field_types-normal_field.
      result = VALUE #( metadata-fields[ name = to_lower( fieldname ) ] OPTIONAL ).
    ELSEIF type = zif_qdrt_c_global=>c_field_types-parameter.
      result = VALUE #( metadata-parameters[ name = to_lower( fieldname ) ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_fields_metadata.
    read_metadata( ).
    result = metadata-fields.
  ENDMETHOD.


  METHOD fill_fields_metadata.

    LOOP AT fields_cds ASSIGNING FIELD-SYMBOL(<field_cds>) WHERE datatype <> 'CLNT'.
      DATA(dtel_info) = zcl_qdrt_metadata_util=>get_dtel_info( <field_cds>-rollname ).
      DATA(field_info) = CORRESPONDING zif_qdrt_ty_global=>ty_field_info(
        dtel_info MAPPING has_fix_values     = valexi
                          short_description  = scrtext_s
                          medium_description = scrtext_m
                          long_description   = scrtext_l
                          field_text         = fieldtext
                          has_value_help     = f4availabl
                          is_lowercase       = lowercase ).
      IF dtel_info IS INITIAL.
        field_info-field_text = <field_cds>-quickinfo.
      ENDIF.

      field_info-name = <field_cds>-fieldname.
      field_info-datatype = <field_cds>-datatype.
      field_info-is_key = <field_cds>-keyflag.
      field_info-decimals = <field_cds>-decimals.
      field_info-length = <field_cds>-leng.
      metadata-fields = VALUE #( BASE metadata-fields (
        zcl_qdrt_metadata_util=>convert_to_field_metadata(
          field_info  = field_info
          entity_name = entity_name ) ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_param_metadata.
    DATA:
      type_info TYPE dfies.

    CHECK params IS NOT INITIAL.

    LOOP AT params ASSIGNING FIELD-SYMBOL(<param>).
      CLEAR type_info.
      DATA(param_info) = VALUE zif_qdrt_ty_global=>ty_field_info(
        name               = <param>-parametername
        datatype           = <param>-datatype
        decimals           = <param>-decimals
        length             = <param>-leng
        rollname           = <param>-rollname
        field_text         = <param>-ddtext ).
      IF <param>-rollname IS NOT INITIAL.
        type_info = zcl_qdrt_metadata_util=>get_dtel_info( rollname = <param>-rollname ).
        param_info-domname            = type_info-domname.
        param_info-has_fix_values     = type_info-valexi.
        param_info-short_description  = type_info-scrtext_s.
        param_info-medium_description = type_info-scrtext_m.
        param_info-long_description   = type_info-scrtext_l.
        param_info-has_value_help     = type_info-f4availabl.
        param_info-ref_field          = type_info-reffield.
        param_info-ref_table          = type_info-reftable.
        param_info-checktable         = type_info-checktable.
        param_info-is_lowercase       = type_info-lowercase.
      ENDIF.

      metadata-parameters = VALUE #( BASE metadata-parameters (
        zcl_qdrt_metadata_util=>convert_to_field_metadata(
          entity_name = entity_name
          field_info  = param_info ) ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD read_metadata.
    CHECK:
      zif_qdrt_entity_metadata_prov~entity_exists( ),
      metadata IS INITIAL.

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
    metadata-entity = VALUE #(
      name        = header_info-strucobjn
      raw_name    = header_info-strucobjn_raw
      description = header_info-ddtext ).

    ddl_view_name = nodes[ 1 ]-dbtabname.

    fill_fields_metadata( fields_cds ).
    fill_param_metadata( parameters ).
  ENDMETHOD.

ENDCLASS.
