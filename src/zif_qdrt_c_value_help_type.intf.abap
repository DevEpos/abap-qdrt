"! <p class="shorttext synchronized" lang="en">Types of Value Help</p>
INTERFACE zif_qdrt_c_value_help_type
  PUBLIC .

  CONSTANTS:
    check_table        TYPE string VALUE 'CheckTable',
    fix_values         TYPE string VALUE 'DomainFixValues',
    ddic_sh            TYPE string VALUE 'DDICSearchHelp',
    elementary_ddic_sh TYPE string VALUE 'ElementaryDDICSearchHelp',
    collective_ddic_sh TYPE string VALUE 'CollectiveDDICSearchHelp'.
ENDINTERFACE.
