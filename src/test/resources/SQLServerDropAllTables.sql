
-- drop all AQA tables in SQL Server

-- To list tables in SQL Server:
--    SELECT * FROM SYSOBJECTS WHERE xtype = 'U';

drop table "AQAmsDV"."dbo"."vmat";
drop table "AQAmsDV"."dbo"."leafPosition";
drop table "AQAmsDV"."dbo"."symmetryAndFlatness";
drop table "AQAmsDV"."dbo"."Rect";
drop table "AQAmsDV"."dbo"."bbByEPIDComposite";
drop table "AQAmsDV"."dbo"."bbByEPID";
drop table "AQAmsDV"."dbo"."bbByCBCT";
drop table "AQAmsDV"."dbo"."wedgePoint";
drop table "AQAmsDV"."dbo"."collimatorPosition";
drop table "AQAmsDV"."dbo"."centerDose";
drop table "AQAmsDV"."dbo"."badPixel";
drop table "AQAmsDV"."dbo"."collimatorCentering";
drop table "AQAmsDV"."dbo"."metadataCheck";
drop table "AQAmsDV"."dbo"."diffBaselineTrans";
drop table "AQAmsDV"."dbo"."diffBaselineOpen";
drop table "AQAmsDV"."dbo"."rSquared";
drop table "AQAmsDV"."dbo"."leafTransmission";
drop table "AQAmsDV"."dbo"."epidCenterCorrection";
drop table "AQAmsDV"."dbo"."leafOffsetCorrection";
drop table "AQAmsDV"."dbo"."centralAxis";
drop table "AQAmsDV"."dbo"."baselineContent";
drop table "AQAmsDV"."dbo"."baseline";
drop table "AQAmsDV"."dbo"."dicomAnonymous";
drop table "AQAmsDV"."dbo"."dicomSeries";
drop table "AQAmsDV"."dbo"."outputFiles";
drop table "AQAmsDV"."dbo"."output";
drop table "AQAmsDV"."dbo"."maintenanceRecord";
drop table "AQAmsDV"."dbo"."inputFiles";
drop table "AQAmsDV"."dbo"."input";
drop table "AQAmsDV"."dbo"."machineBeamEnergy";
drop table "AQAmsDV"."dbo"."machine";
drop table "AQAmsDV"."dbo"."epid";
drop table "AQAmsDV"."dbo"."multileafCollimator";
drop table "AQAmsDV"."dbo"."machineType";
drop table "AQAmsDV"."dbo"."systemModification";
drop table "AQAmsDV"."dbo"."procedure";
drop table "AQAmsDV"."dbo"."user";
drop table "AQAmsDV"."dbo"."institution";
