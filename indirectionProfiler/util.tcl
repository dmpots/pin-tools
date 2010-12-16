#!/usr/bin/tclsh

package require sqlite3

proc Process {f} {
    puts "proc{$f}"
    sqlite3 db "$f"

    db eval {SELECT type, category, COUNT(type) AS static_count, SUM(CallTargets.count) AS dynamic_count FROM CallSites, CallTargets WHERE (CallSites.rowid = CallTargets.callsite) GROUP BY type, category ORDER BY dynamic_count, type, category LIMIT 1;} values {
	parray values
	puts ""
    }
    
    
    
    db close
}

foreach f $argv {
    Process $f
}