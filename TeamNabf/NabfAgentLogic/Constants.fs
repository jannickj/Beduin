namespace NabfAgentLogic
    module Constants = 
        
        ///////////////////////////////
        /// Agent constants
        ///////////////////////////////
        let STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER0 = 0.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER1 = 20.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER2 = 30.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER1 = 50.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER2 = 70.0

        //lower means distance between agent and job matters less
        let DISTANCE_TO_OCCUPY_JOB_MOD = 0.1
        let DISTANCE_TO_REPAIR_JOB_MOD = 0.01
        let DISTANCE_TO_DISRUPT_JOB_MOD = 0.1
        let DISTANCE_TO_ATTACK_JOB_MOD = 0.1

        let DESIRE_COST_OF_MOVING_THROUGH_ONE_ENEMY_NODE = 0.02 //job value will be multiplied by 1.0*this_constant. This constant may not be negative and not exceed 1.0

        //don't change these. Modify the above constants as what they actually do is clearly understood
        let REPAIRER_OCCUPYJOB_MOD = -0.0
        let SENTINEL_OCCUPYJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER1 / 10.0)
        let INSPECTOR_OCCUPYJOB_MOD = 10.0
        let SABOTEUR_OCCUPYJOB_MOD = -0.0
        let EXPLORER_OCCUPYJOB_MOD = 10.0

        //don't change these. Modify the above constants as what they actually do is clearly understood
        let REPAIRER_DISRUPTJOB_MOD = -0.0
        let SENTINEL_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER2 / 10.0)
        let INSPECTOR_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER1 / 10.0)
        let SABOTEUR_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER0 / 10.0)
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
        let MINIMUM_VALUE_VALUE = 8

        let SIMULATED_EDGE_COST = 1

        let RECHARGE_FACTOR = 0.5

        let EDGE_COST_MAX = 9

        let ACTION_COST_MAX = 9
        let ACTION_COST_CHEAP = 1
        let ACTION_COST_EXPENSIVE = 2
        let ACTION_COST_DISABLED = 3
       
        let OUR_TEAM = "Nabf"
        let EXPLORE_FACTOR_LIGHT = 0.7
        let MAX_PLANNING_TIME_MS = 100L

        let NUMBER_OF_AGENTS = 28

