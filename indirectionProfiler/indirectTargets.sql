.headers ON
.mode csv


-- This query will associate each call target with the appropriate
-- callsite; it also associates the appropriate image and routine
-- labes with the source and target addresses (if found).  The query
-- is saved as a temporary table.
CREATE TEMPORARY TABLE Calls AS
SELECT CallSites.type AS type,CallSites.category AS category,CallSites.addr AS source,target,count,
       (SELECT SUM(count) from CallTargets as InnerCalls WHERE CallTargets.callsite=InnerCalls.callsite) as sourcecount,
       ROUND((100.0*count) / (1.0*(SELECT SUM(count) from CallTargets AS InnerCalls WHERE CallTargets.callsite=InnerCalls.callsite)),4) AS targetshare,
       100.0 - ROUND((100.0*(SELECT COALESCE(SUM(count), 0) FROM CallTargets AS CallsInner WHERE CallTargets.callsite=CallsInner.callsite AND CallsInner.count<=CallTargets.count) / (1.0*((SELECT SUM(count) from CallTargets as InnerCalls WHERE CallTargets.callsite=InnerCalls.callsite)))),4) as CumulativeShare,
       SourceImageMap.label AS sourceimage,
       SourceRoutineMap.label AS sourceroutine,
       TargetImageMap.label AS targetimage,
       TargetRoutineMap.label AS targetroutine
       FROM CallTargets
             JOIN CallSites ON (CallTargets.callsite = CallSites.rowid)
       	     LEFT OUTER JOIN RoutineMap AS SourceRoutineMap
	            	     ON (SourceRoutineMap.rowid = CallSites.Routine)
       	     LEFT OUTER JOIN RoutineMap AS TargetRoutineMap
	            	     ON (TargetRoutineMap.rowid = CallTargets.Routine)
       	     LEFT OUTER JOIN ImageMap AS SourceImageMap
	            	     ON (SourceImageMap.rowid = CallSites.Image)
       	     LEFT OUTER JOIN ImageMap AS TargetImageMap
	            	     ON (TargetImageMap.rowid = CallTargets.Image)
       ORDER BY count DESC
;

-- This command aggregates the calls based on source address; it
-- computes the total number of calls from a callsite and the total
-- number of call targets
-- SELECT type, category, source, COUNT(target) as targets, SUM(count) as dynamic_count
--        FROM Calls
--        WHERE type="IndirectBranch"
--        AND category<>"RET"
-- --       AND CumulativeShare <= 90.0
--        GROUP BY source
--        ORDER BY dynamic_count DESC
-- --       LIMIT 10
-- ;

CREATE TEMPORARY TABLE TargetCount100 AS
SELECT type, category, source, COUNT(target) as targets, SUM(count) as dynamic_count
       FROM Calls
       WHERE type="IndirectBranch"
       AND category<>"RET"
--       AND CumulativeShare <= 90.0
       GROUP BY source
       ORDER BY dynamic_count DESC;

CREATE TEMPORARY TABLE TargetCount90 AS
SELECT type, category, source, COUNT(target) as targets, SUM(count) as dynamic_count
       FROM Calls
       WHERE type="IndirectBranch"
       AND category<>"RET"
       AND CumulativeShare <= 90.0
       GROUP BY source
       ORDER BY dynamic_count DESC;

SELECT "100" as "Subset",
       (SELECT count(targets) FROM TargetCount100 where targets>0 AND targets <=1) as "1",
       (SELECT count(targets) FROM TargetCount100 where targets>1 AND targets <=2) as "2",
       (SELECT count(targets) FROM TargetCount100 where targets>2 AND targets <=4) as "4",
       (SELECT count(targets) FROM TargetCount100 where targets>4 AND targets <=8) as "8",
       (SELECT count(targets) FROM TargetCount100 where targets>8) as "More"
;
SELECT "90" as "Subset",
       (SELECT count(targets) FROM TargetCount90 where targets>0 AND targets <=1) as "1",
       (SELECT count(targets) FROM TargetCount90 where targets>1 AND targets <=2) as "2",
       (SELECT count(targets) FROM TargetCount90 where targets>2 AND targets <=4) as "4",
       (SELECT count(targets) FROM TargetCount90 where targets>4 AND targets <=8) as "8",
       (SELECT count(targets) FROM TargetCount90 where targets>8) as "More"
;
