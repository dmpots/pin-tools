.headers ON
.mode column


-- This query will associate each call target with the appropriate callsite; it also associates the appropriate image and routine labes with the source and target addresses (if found).  The query is saved as a temporary table.
--DROP VIEW IF EXISTS Calls;
--CREATE VIEW Calls AS 
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

-- This command will aggregate the call sites based on type and category
SELECT type, category, COUNT(source) AS static_count, SUM(count) AS dynamic_count
       FROM Calls
       GROUP BY type, category
       ORDER BY dynamic_count, type, category;

-- This command will aggregate the call sites based on the number of targets
SELECT type, targets, SUM(static_count) AS "static_count", SUM(dynamic_count) AS "dynamic_count" FROM (
SELECT type,category,addr,1 as "static_count", COUNT(target) as "targets",SUM(count) as "dynamic_count"
       FROM CallSites,CallTargets
       WHERE CallSites.rowid = CallTargets.callsite AND CallSites.type="IndirectBranch" AND CallSites.category <> "RET"
       GROUP BY addr
       ORDER BY dynamic_count DESC)
GROUP BY targets
ORDER BY targets;

-- This command will compute the average targets for each indirect callsite
SELECT type,category,addr,COUNT(target) as "targets",SUM(count) as "dynamic_count", SUM(count) / COUNT(target) AS "CallsPerTarget"
       FROM CallSites,CallTargets
       	    LEFT JOIN ImageMap ON (ImageMap.rowid = CallSites.Image)
       	    LEFT JOIN RoutineMap ON (RoutineMap.rowid = CallSites.Routine)
       WHERE CallSites.rowid = CallTargets.callsite AND CallSites.type="IndirectBranch" AND CallSites.category <> "RET"
       GROUP BY addr
       ORDER BY dynamic_count DESC
       LIMIT 10
;

-- This command aggregates the calls based on source address; it
-- computes the total number of calls from a callsite and the total
-- number of call targets
SELECT type, category, source, COUNT(target) as targets, SUM(count) as dynamic_count
       FROM Calls
       WHERE type="IndirectBranch"
       GROUP BY source
       ORDER BY dynamic_count DESC
       LIMIT 10
;

-- This query prints out every callsite/calltarget pair and shows the
-- "share" each target represents in relation to the total number of
-- call targets for a callsite (weighted by the number of calls to
-- each call target).  The cumulative share is also shown, which can
-- be used for selecting the most "dominant" call targets.  For
-- example, you can query for the call targets that comprise 90% of
-- the calls made at that call site.
SELECT type, category, source, target, count, targetshare, CumulativeShare
       FROM Calls
       WHERE type="IndirectBranch"
       ORDER BY source
       LIMIT 10
;       

SELECT type, category, source, COUNT(target) as targets, SUM(count) as dynamic_count
       FROM Calls
       WHERE type="IndirectBranch"
       AND CumulativeShare <= 90.0
       GROUP BY source
       ORDER BY targets DESC
--       LIMIT 10
;


