namespace NabfAgentLogic
    module Constants = 
        
        ///////////////////////////////
        /// Agent constants
        ///////////////////////////////

        //lower means distance between agent and job matters less
        let DISTANCE_TO_OCCUPY_JOB_MOD = 1.0
        let DISTANCE_TO_REPAIR_JOB_MOD = 2.0
        let DISTANCE_TO_DISRUPT_JOB_MOD = 1.0
        let DISTANCE_TO_ATTACK_JOB_MOD = 1.0

        let JOB_IMPORTANCE_MODIFIER_OCCUPY = 10.0
        let JOB_IMPORTANCE_MODIFIER_ATTACK = 1.0

        let DESIRE_COST_OF_MOVING_THROUGH_ONE_ENEMY_NODE = 0.02 //distance to job will be (dist + number_of_enemy_nodes*this_constant), meaning
                                                                //nodes with enemies on them on the route will be considered more expensive,
                                                                // so that agents who have a safe path will desire a job more
                                                                //This constant may not be negative and not exceed 1.0

        //Use these if specific roles should have preference on a job. Compare it to the DISTANCE_TO modifier. 
        //If role X should have preference over role Y at distance from job Z, given DIST mod of 1.0, then be sure that ROLE mod of X is Z larger than the ROLE mod of Y
        let REPAIRER_OCCUPYJOB_MOD = -0.0
        let SENTINEL_OCCUPYJOB_MOD = 10.0
        let INSPECTOR_OCCUPYJOB_MOD = 10.0
        let SABOTEUR_OCCUPYJOB_MOD = -0.0
        let EXPLORER_OCCUPYJOB_MOD = 5.0

        //Use these if specific roles should have preference on a job. Compare it to the DISTANCE_TO modifier. 
        //If role X should have preference over role Y at distance from job Z, given DIST mod of 1.0, then be sure that ROLE mod of X is Z larger than the ROLE mod of Y
        let REPAIRER_DISRUPTJOB_MOD = -0.0
        let SENTINEL_DISRUPTJOB_MOD = 10.0
        let INSPECTOR_DISRUPTJOB_MOD = 10.0
        let SABOTEUR_DISRUPTJOB_MOD = 10.0
        let EXPLORER_DISRUPTJOB_MOD = 10.0

        //not used as we only have one role to apply for these job types
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