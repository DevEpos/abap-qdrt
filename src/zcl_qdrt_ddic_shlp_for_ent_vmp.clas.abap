"! <p class="shorttext synchronized" lang="en">Metadata provider for DDIC value help at field level</p>
CLASS zcl_qdrt_ddic_shlp_for_ent_vmp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_qdrt_vh_metadata_provider.
    METHODS:
      constructor
        IMPORTING
          entity_name TYPE zif_qdrt_ty_global=>ty_entity_name
          entity_type TYPE zif_qdrt_ty_global=>ty_entity_type
          fieldname   TYPE fieldname
          field_type  TYPE zif_qdrt_ty_global=>ty_field_type
        RAISING
          zcx_qdrt_appl_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_metadata.
             INCLUDE TYPE zif_qdrt_ty_global=>ty_vh_metadata.
    TYPES included_value_helps TYPE zif_qdrt_ty_global=>ty_t_vh_metadata.
    TYPES: END OF ty_metadata.
    DATA:
      exists         TYPE abap_bool VALUE abap_undefined,
      metadata       TYPE ty_metadata,
      entity_name    TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type    TYPE zif_qdrt_ty_global=>ty_entity_type,
      fieldname      TYPE fieldname,
      field_type     TYPE zif_qdrt_ty_global=>ty_field_type,
      shlp_name      TYPE shlpname,
      shlp_tabname   TYPE tabname,
      shlp_fieldname TYPE fieldname.

    METHODS:
      read_metadata,
      resolve_field_information,
      retrieve_child_vh_info,
      handle_collection_vh.
ENDCLASS.



CLASS zcl_qdrt_ddic_shlp_for_ent_vmp IMPLEMENTATION.


  METHOD constructor.
    me->entity_name = entity_name.
    me->entity_type = entity_type.
    me->fieldname = fieldname.
    me->field_type = field_type.
    read_metadata( ).
  ENDMETHOD.


  METHOD zif_qdrt_metadata_provider~entity_exists.
    result = exists.
  ENDMETHOD.


  METHOD zif_qdrt_metadata_provider~get_metadata.
    result = REF #( metadata ).
  ENDMETHOD.


  METHOD read_metadata.
    DATA:
      shlp_description TYPE shlp_descr.

    resolve_field_information( ).

    IF shlp_tabname IS INITIAL.
      exists = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
      EXPORTING
        tabname           = shlp_tabname
        fieldname         = shlp_fieldname
      IMPORTING
        shlp              = shlp_description
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      exists = abap_false.
      RETURN.
    ENDIF.

    metadata = CORRESPONDING #( zcl_qdrt_metadata_util=>convert_to_vh_metadata(
      shlp_descr   = shlp_description
      source_tab   = shlp_tabname
      source_field = shlp_fieldname ) ).

    IF shlp_description-intdescr-issimple = abap_false.
      handle_collection_vh( ).
    ENDIF.

  ENDMETHOD.


  METHOD resolve_field_information.
    IF field_type = zif_qdrt_c_global=>c_field_types-parameter.
      " Use the data element for determination of the assigned value help of the parameter
      SELECT SINGLE rollname
        FROM dd10b
        WHERE strucobjn = @entity_name
          AND parametername = @fieldname
        INTO @shlp_tabname.
    ELSE.
      shlp_tabname = entity_name.
      shlp_fieldname = fieldname.
    ENDIF.
  ENDMETHOD.


  METHOD handle_collection_vh.
    DATA:
      shlp_description TYPE shlp_descr.

    CLEAR: metadata-fields,
           metadata-output_fields,
           metadata-filter_fields.

    retrieve_child_vh_info( ).

    IF metadata-included_value_helps IS INITIAL.
      CLEAR metadata.
      exists = abap_false.
      RETURN.
    ENDIF.

    data(first_child_vh_simple) = metadata-included_value_helps[ 1 ].

    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = first_child_vh_simple-value_help_name
        shlptype = 'SH'
      IMPORTING
        shlp     = shlp_description.

    DATA(child_vh_metadata) = zcl_qdrt_metadata_util=>convert_to_vh_metadata( shlp_descr = shlp_description ).
    child_vh_metadata-description = first_child_vh_simple-description.
    child_vh_metadata-type = first_child_vh_simple-type.

    IF lines( metadata-included_value_helps ) = 1.
      " only 1 child search help, so overwrite the collective with a single elementary search help
      DATA(token_key) = metadata-token_key_field.
      CLEAR metadata.
      metadata = CORRESPONDING #( child_vh_metadata ).
      metadata-token_key_field = token_key.
    ELSE.
      metadata-included_value_helps[ 1 ] = CORRESPONDING #( child_vh_metadata ).
    ENDIF.
  ENDMETHOD.


  METHOD retrieve_child_vh_info.
    DATA:
      child_search_helps TYPE TABLE OF ddshdescr.

    CALL FUNCTION 'F4IF_TOPLEVEL_SEARCHHELPS'
      EXPORTING
        shlpname      = metadata-value_help_name
      TABLES
        ddshdescr_tab = child_search_helps.

    SORT child_search_helps BY shposition.

    " determine the names for the search helps
    DATA(shlp_texts) = zcl_qdrt_text_util=>get_short_texts( VALUE #(
      FOR shlp IN child_search_helps
      ( type = 'SHLP'
        name = shlp-shlpname ) ) ).

    LOOP AT child_search_helps ASSIGNING FIELD-SYMBOL(<child_shlp>).
      APPEND VALUE #(
        value_help_name = <child_shlp>-shlpname
        type            = zif_qdrt_c_value_help_type=>elementary_ddic_sh
        description     = VALUE #( shlp_texts[
          KEY obj_name obj_name = <child_shlp>-shlpname
                       object   = 'SHLP' ]-stext ) ) TO metadata-included_value_helps.
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
