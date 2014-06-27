namespace NabfAgentLogic
    module Constants = 
        
        (*
        *   Agent Properties
        *)

        let EXPLORER_MAX_ENERGY = 35
        let REPAIRER_MAX_ENERGY = 25
        let SABOTEUR_MAX_ENERGY = 20
        let SENTINEL_MAX_ENERGY = 30
        let INSPECTOR_MAX_ENERGY = 25

        let EXPLORER_MAX_ENERGY_DISABLED = 35
        let REPAIRER_MAX_ENERGY_DISABLED = 25
        let SABOTEUR_MAX_ENERGY_DISABLED = 20
        let SENTINEL_MAX_ENERGY_DISABLED = 30
        let INSPECTOR_MAX_ENERGY_DISABLED = 25

        let EXPLORER_MAX_HEALTH = 4
        let REPAIRER_MAX_HEALTH = 6
        let SABOTEUR_MAX_HEALTH = 3
        let SENTINEL_MAX_HEALTH = 1
        let INSPECTOR_MAX_HEALTH = 6

        let EXPLORER_VISION_RANGE = 2
        let REPAIRER_VISION_RANGE = 1
        let SABOTEUR_VISION_RANGE = 1
        let SENTINEL_VISION_RANGE = 3
        let INSPECTOR_VISION_RANGE = 1

        let EXPLORER_STRENGTH = 0
        let REPAIRER_STRENGTH = 0
        let SABOTEUR_STRENGTH = 3
        let SENTINEL_STRENGTH = 0
        let INSPECTOR_STRENGTH = 0
        
        ///////////////////////////////
        /// Agent constants
        ///////////////////////////////
        let REPAIRER_REPAIR_PRIORITY = 10
        let SENTINEL_REPAIR_PRIORITY = 0
        let INSPECTOR_REPAIR_PRIORITY = 0
        let SABOTEUR_REPAIR_PRIORITY = 20
        let EXPLORER_REPAIR_PRIORITY = 1

        //lower means distance between agent and job matters less
        let DISTANCE_TO_OCCUPY_JOB_MOD = 1.0
        let DISTANCE_TO_REPAIR_JOB_MOD = 1.0
        let DISTANCE_TO_DISRUPT_JOB_MOD = 1.0
        let DISTANCE_TO_ATTACK_JOB_MOD = 1.0

        let JOB_IMPORTANCE_MODIFIER_OCCUPY = 10.0
        let JOB_IMPORTANCE_MODIFIER_ATTACK = 1.0
        let JOB_IMPORTANCE_MODIFIER_REPAIR = 1.0

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
        let EXPLORER_OCCUPYJOB_MOD = 10.0

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

        let DEFENSE_IMPORTANCE_MODIFIER = 2
        let ATTACK_IMPORTANCE_MODIFIER = 1
        let VALUE_DECAY_PER_TURN = 2.0
        let JOB_AGE_VALUE_DECREASE_FACTOR = 2.0

        //value related to sentinel survey
        let SURVEY_NEEDED_FACTOR_RANGE1 = 0.6
        let SURVEY_NEEDED_FACTOR_RANGE2 = 0.8
        let SURVEY_NEEDED_FACTOR_RANGE1_WHILE_ON_2 = 0.4
        let SURVEY_MY_NODE_UNTIL_THIS_TURN_IF_NEEDED = 50
        let ENERGY_FACTOR_TO_PREFER_SURVEY_OVER_RECHARGE = 0.75

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
       
        let MAIL_EXPIRATION = 1

        let mutable OUR_TEAM = "Nabf"
        let EXPLORE_FACTOR_DONE_EXPLORING = 0.98
        let PROBE_FACTOR_LIGHT = 0.98
        let MAX_PLANNING_TIME_MS = 1000L

        let NUMBER_OF_AGENTS = 28
        let INFINITE_HEURISTIC = 10000

        let PERCEPT_TIME_BUFFER = 2000.0

        let MINIMUM_ROLE_CERTAINTY = 50
