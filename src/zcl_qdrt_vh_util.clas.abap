"! <p class="shorttext synchronized" lang="en">Utility for value helps</p>
CLASS zcl_qdrt_vh_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Converts shlp description to vh metadata</p>
      convert_to_vh_metadata
        IMPORTING
          source_tab    TYPE tabname OPTIONAL
          source_field  TYPE fieldname OPTIONAL
          shlp_descr    TYPE shlp_descr
        RETURNING
          VALUE(result) TYPE zif_qdrt_ty_global=>ty_vh_metadata,
      "! <p class="shorttext synchronized" lang="en">Creates result table for value help definition</p>
      create_result_table_for_vh
        IMPORTING
          vh            TYPE shlp_descr
        RETURNING
          VALUE(result) TYPE REF TO data,
      "! <p class="shorttext synchronized" lang="en">Selects data via value help API</p>
      select_data_via_vh
        IMPORTING
          max_rows  TYPE i DEFAULT 200
          vh        TYPE shlp_descr
          vh_result TYPE REF TO data,
      "! <p class="shorttext synchronized" lang="en">Sets custom id field for vh metadata for encoded content</p>
      "! SAPUI5 expects the content of the key column to be encoded at some places
      add_encoded_id_field_to_meta
        CHANGING
          vh_meta TYPE zif_qdrt_ty_global=>ty_vh_metadata.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_vh_util IMPLEMENTATION.


  METHOD convert_to_vh_metadata.
    FIELD-SYMBOLS:
      <shlp_field> TYPE ddshfprop.

    result-value_help_name = shlp_descr-shlpname.
    IF shlp_descr-intdescr-issimple = abap_false.
      result-type = zif_qdrt_c_value_help_type=>collective_ddic_sh.
    ELSE.
      IF shlp_descr-shlptype = 'CT'.
        result-type = zif_qdrt_c_value_help_type=>check_table.
      ELSE.
        result-type = zif_qdrt_c_value_help_type=>elementary_ddic_sh.
      ENDIF.
    ENDIF.

    result-source_tab = source_tab.
    result-source_field = source_field.

    IF source_tab IS NOT INITIAL.
      ASSIGN shlp_descr-interface[ valtabname = source_tab
                                   valfield   = source_field ] TO FIELD-SYMBOL(<target_field>).
      IF sy-subrc = 0.
        result-token_key_field = to_lower( <target_field>-shlpfield ).
      ENDIF.
    ENDIF.

    " create result table from search help field definition
    LOOP AT shlp_descr-fieldprop ASSIGNING FIELD-SYMBOL(<sh_field_prop>) WHERE shlplispos > 0
                                                                            OR shlpselpos > 0
                                                                            OR shlpoutput = abap_true.

      ASSIGN shlp_descr-fielddescr[ fieldname = <sh_field_prop>-fieldname ] TO FIELD-SYMBOL(<sh_field_descr>).

      CHECK sy-subrc = 0.

      APPEND zcl_qdrt_metadata_util=>convert_to_field_metadata(
        field_info  = CORRESPONDING #( <sh_field_descr>
          MAPPING has_fix_values     = valexi
                  short_description  = scrtext_s
                  medium_description = scrtext_m
                  long_description   = scrtext_l
                  field_text         = fieldtext
                  is_lowercase       = lowercase
                  length             = leng
                  name               = fieldname ) ) TO result-fields.
    ENDLOOP.

    " collect output and filter fields
    DATA(fields) = shlp_descr-fieldprop.
    SORT fields BY shlplispos.

    LOOP AT fields ASSIGNING <shlp_field> WHERE shlplispos > 0.
      APPEND to_lower( <shlp_field>-fieldname ) TO result-output_fields.
    ENDLOOP.

    SORT fields BY shlpselpos.

    LOOP AT fields ASSIGNING <shlp_field> WHERE shlpselpos > 0.
      APPEND to_lower( <shlp_field>-fieldname ) TO result-filter_fields.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_result_table_for_vh.
    DATA: vh_result_tab_comps TYPE abap_component_tab.

    " create result table from search help field definition
    LOOP AT vh-fieldprop ASSIGNING FIELD-SYMBOL(<sh_field_prop>) WHERE shlplispos > 0
                                                                    OR shlpoutput = abap_true.
      ASSIGN vh-fielddescr[ fieldname = <sh_field_prop>-fieldname ] TO FIELD-SYMBOL(<sh_field_descr>).
      CHECK sy-subrc = 0.

      vh_result_tab_comps = VALUE #( BASE vh_result_tab_comps
        ( name = <sh_field_descr>-fieldname
          type = CAST #(
            cl_abap_typedescr=>describe_by_name( <sh_field_descr>-rollname ) ) ) ).

    ENDLOOP.

    IF vh_result_tab_comps IS INITIAL.
      RETURN.
    ENDIF.

    DATA(sh_result_structdescr) = cl_abap_structdescr=>create( p_components = vh_result_tab_comps ).
    DATA(sh_result_tabdescr) = cl_abap_tabledescr=>create( p_line_type = sh_result_structdescr ).

    CREATE DATA result TYPE HANDLE sh_result_tabdescr.
  ENDMETHOD.


  METHOD select_data_via_vh.
    DATA: vh_return TYPE TABLE OF ddshretval.
    FIELD-SYMBOLS: <vh_result> TYPE STANDARD TABLE.

    CHECK:
      vh_result IS BOUND,
      vh-fielddescr IS NOT INITIAL.

    ASSIGN vh_result->* TO <vh_result>.

    DATA(l_vh) = vh.
    " clear any conversion exits from field definitions so the internal format is returned
    LOOP AT l_vh-fielddescr ASSIGNING FIELD-SYMBOL(<field_descriptor>) WHERE convexit IS NOT INITIAL.
      CLEAR: <field_descriptor>-convexit.
    ENDLOOP.

    " F4IF_SELECT_VALUES only returns values in fields that are marked as 'exporting' but
    "  we expect also values in the fields that are marked as list fields
    LOOP AT l_vh-fieldprop ASSIGNING FIELD-SYMBOL(<field_prop>) WHERE shlplispos > 0.
      <field_prop>-shlpoutput = abap_true.
    ENDLOOP.

    CALL FUNCTION 'F4IF_SELECT_VALUES'
      EXPORTING
        shlp           = l_vh
        maxrows        = max_rows
        call_shlp_exit = abap_true
*  IMPORTING
*       maxrows_exceeded =
      TABLES
        return_tab     = vh_return.

    DATA(last_index) = 0.

    LOOP AT vh_return ASSIGNING FIELD-SYMBOL(<vh_return_field>).
      IF last_index <> <vh_return_field>-recordpos.
        last_index = <vh_return_field>-recordpos.
        APPEND INITIAL LINE TO <vh_result> ASSIGNING FIELD-SYMBOL(<vh_result_line>).
      ENDIF.

      IF <vh_result_line> IS ASSIGNED.
        ASSIGN COMPONENT <vh_return_field>-fieldname OF STRUCTURE <vh_result_line> TO FIELD-SYMBOL(<result_field>).
        IF sy-subrc = 0.
          <result_field> = <vh_return_field>-fieldval.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_encoded_id_field_to_meta.
    ASSIGN vh_meta-fields[ name = vh_meta-token_key_field ] TO FIELD-SYMBOL(<token_key_field>).
    IF sy-subrc = 0 AND <token_key_field>-type = zif_qdrt_c_edm_types=>string.
      vh_meta-token_key_field = to_lower( zif_qdrt_c_global=>c_custom_field_names-encoded_token_key_field ).
      INSERT VALUE #(
        name = vh_meta-token_key_field
        type = zif_qdrt_c_edm_types=>string ) INTO vh_meta-fields INDEX 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
