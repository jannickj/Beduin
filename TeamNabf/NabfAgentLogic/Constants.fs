namespace NabfAgentLogic
    module Constants = 
        
        ///////////////////////////////
        /// Agent constants
        ///////////////////////////////
        let STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER0 = 0.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER1 = 7.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER2 = 10.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER1 = 5.0
        let STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER2 = 10.0

        //lower means distance between agent and job matters less
        let DISTANCE_TO_OCCUPY_JOB_MOD = 0.7
        let DISTANCE_TO_REPAIR_JOB_MOD = 2.0
        let DISTANCE_TO_DISRUPT_JOB_MOD = 0.7
        let DISTANCE_TO_ATTACK_JOB_MOD = 0.7

        let DESIRE_COST_OF_MOVING_THROUGH_ONE_ENEMY_NODE = 0.02 //distance to job will be (dist + number_of_enemy_nodes*this_constant), meaning
                                                                //nodes with enemies on them on the route will be considered more expensive,
                                                                // so that agents who have a safe path will desire a job more
                                                                //This constant may not be negative and not exceed 1.0

        //don't change these. Modify the above constants as what they actually do is clearly understood
        let REPAIRER_OCCUPYJOB_MOD = -0.0
        let SENTINEL_OCCUPYJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_OCCUPY_TIER1 / 10.0)
        let INSPECTOR_OCCUPYJOB_MOD = 10.0
        let SABOTEUR_OCCUPYJOB_MOD = -0.0
        let EXPLORER_OCCUPYJOB_MOD = 5.0

        //don't change these. Modify the above constants as what they actually do is clearly understood
        let REPAIRER_DISRUPTJOB_MOD = -0.0
        let SENTINEL_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER2 / 10.0)
        let INSPECTOR_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER1 / 10.0)
        let SABOTEUR_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER0 / 10.0)
        let EXPLORER_DISRUPTJOB_MOD = 10.0 + (STEPS_BEFORE_ROLE_DONT_MATTER_DISRUPT_TIER1 / 10.0)

        
        let SABOTEUR_ATTACKJOB_MOD = 0.0


        let REPAIRER_REPAIRJOB_MOD = 0.0

        let SPONTANOUS_REPAIR_PERCENTAGE = 0.50

        let MIN_NODE_VALUE_TO_POST_ATTACK = 8

        ///////////////////////////////
        /// Other constants
        ///////////////////////////////
        let MINIMUM_EDGE_COST = 1
        let MAXIMUM_EDGE_COST = 9

        let ZONE_ORIGIN_VALUE = 9
        let ZONE_BORDER_VALUE = 7
        let SOME_VALUE_VALUE = 8
        let LITTLE_VALUE_VALUE = 6
        let LEAST_VALUE_VALUE = 4
        let MINIMUM_VALUE_VALUE = 2

        let SIMULATED_EDGE_COST = 1

        let RECHARGE_FACTOR = 0.5

        let ACTION_COST_MAX = 9
        let ACTION_COST_CHEAP = 1
        let ACTION_COST_EXPENSIVE = 2
        let ACTION_COST_DISABLED = 3
       
        let OUR_TEAM = "Nabf"
        let EXPLORE_FACTOR_LIGHT = 0.9
        let MAX_PLANNING_TIME_MS = 1000L

        let NUMBER_OF_AGENTS = 28
        let INFINITE_HEURISTIC = 10000

        let PERCEPT_TIME_BUFFER = 1000.0