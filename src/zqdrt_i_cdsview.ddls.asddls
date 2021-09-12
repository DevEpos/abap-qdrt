@AbapCatalog.sqlViewName: 'ZQDRTICDSV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View'

define view ZQDRT_I_CdsView
  as select from    ddddlsrc      as DDLSource
    inner join      ddldependency as DDL2StrucObj     on  DDLSource.ddlname       = DDL2StrucObj.ddlname
                                                      and DDL2StrucObj.objecttype = 'STOB'
                                                      and DDL2StrucObj.state      = 'A'
    inner join      dd02b         as StructuredObject on DDL2StrucObj.objectname = StructuredObject.strucobjn
    left outer join ddddlsrc02bt  as Text             on  Text.ddlname    = DDLSource.ddlname
                                                      and Text.strucobjn  = StructuredObject.strucobjn
                                                      and Text.as4local   = 'A'
                                                      and Text.ddlanguage = $session.system_language
    inner join      tadir         as Repo             on  DDLSource.ddlname = Repo.obj_name
                                                      and Repo.pgmid        = 'R3TR'
                                                      and Repo.object       = 'DDLS'
{
  key StructuredObject.strucobjn     as EntityId,
      StructuredObject.strucobjn_raw as RawEntityId,
      DDLSource.ddlname              as AltEntityId,
      Text.ddtext                    as Description,
      Repo.devclass                  as DevelopmentPackage,
      Repo.author                    as CreatedBy,
      Repo.created_on                as CreatedDate,
      DDLSource.as4user              as ChangedBy,
      DDLSource.as4date              as ChangedDate,
      'C'                            as Type
}
where
  DDLSource.as4local = 'A'
