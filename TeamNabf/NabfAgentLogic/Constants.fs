namespace NabfAgentLogic
    module Constants = 
        
        ///////////////////////////////
        /// Agent constants
        ///////////////////////////////
        let STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER1 = 20.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER2 = 30.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER1 = 50.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER2 = 70.0

        let REPAIRER_OCCUPYJOB_MOD = -0.0
        let SENTINEL_OCCUPYJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER1 / 10.0)
        let INSPECTOR_OCCUPYJOB_MOD = 10.0
        let SABOTEUR_OCCUPYJOB_MOD = -0.0
        let EXPLORER_OCCUPYJOB_MOD = 10.0


        let REPAIRER_DISRUPTJOB_MOD = -0.0
        let SENTINEL_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER2 / 10.0)
        let INSPECTOR_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER1 / 10.0)
        let SABOTEUR_DISRUPTJOB_MOD = 10.0
        let EXPLORER_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER1 / 10.0)

        
        let SABOTEUR_ATTACKJOB_MOD = 0.0


        let REPAIRER_REPAIRJOB_MOD = 0.0

        let SPONTANOUS_REPAIR_PERCENTAGE = 0.50

        ///////////////////////////////
        /// Other constants
        ///////////////////////////////
        let UNKNOWN_EDGE_COST = 5
        let ZONE_ORIGIN_VALUE = 8
        let ZONE_BORDER_VALUE = 6

        let EXPLORE_FACTOR_LIGHT = 0.7
