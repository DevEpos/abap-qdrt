@AbapCatalog.sqlViewName: 'ZQDRTIDBENT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Database entity (view,table,cds)'

define view ZQDRT_I_DbEntity
  as select from ZQDRT_I_DbTable
{
  key TableName                    as EntityId,
      TableName                    as RawEntityId,
      cast( TableName as ddlname ) as AltEntityId,
      Description,
      DevelopmentPackage,
      CreatedBy,
      CreatedDate,
      ChangedBy,
      ChangedDate,
      Type
}
union select from ZQDRT_I_DbView
{
  key ViewName as EntityId,
      ViewName as RawEntityId,
      ViewName as AltEntityId,
      Description,
      DevelopmentPackage,
      CreatedBy,
      CreatedDate,
      ChangedBy,
      ChangedDate,
      Type
}
union select from ZQDRT_I_CdsView
{
  key EntityId,
      RawEntityId,
      AltEntityId,
      Description,
      DevelopmentPackage,
      CreatedBy,
      CreatedDate,
      ChangedBy,
      ChangedDate,
      Type
}
