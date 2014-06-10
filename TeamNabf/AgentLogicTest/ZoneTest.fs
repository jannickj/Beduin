namespace AgentLogicTest
module ZoneTest =
    open System
    open NUnit.Framework
    open Graphing.Graph
    open NabfAgentLogic.AgentTypes
    open StructureBuilder
    open NabfAgentLogic.Explorer
    open NabfAgentLogic.Planning
    open FsPlanning.Search

    [<TestFixture>]
    type ZoneTest() = 

            [<Test>]
            member this.FindAgentPositions_7NodeGraphWithIsland_Return3Positons() =
                let zone = [ { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList } 
                                    ;{ Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList }
                                    ;{ Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList }
                                    ;{ Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList }
                                    ;{ Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList }
                                    ;{ Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList }
                                    ;{ Identifier = "g"; Value = None; Edges = [(None, "e")] |> Set.ofList }
                                    ]
                let initialGraph =  [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList })
                                    ; ("f", { Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList })
                                    ; ("g", { Identifier = "g"; Value = None; Edges = [(None, "e")] |> Set.ofList })
                                    ] |> Map.ofList
                let placement = findAgentPlacement zone initialGraph
                Assert.AreEqual(3,List.length placement)
                ()
            
            [<Test>]
            member this.FindAgentPositions_7NodeGraphWithoutIsland_Return3Positons() =
                let zone = [ { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList } 
                                    ; { Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList }
                                    ; { Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList }
                                    ; { Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList }
                                    ; { Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList }
                                    ; { Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList }
                                    ; { Identifier = "g"; Value = None; Edges = [(None, "e");(None, "x")] |> Set.ofList }
                                    ]
                let initialGraph =  [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList })
                                    ; ("f", { Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList })
                                    ; ("g", { Identifier = "g"; Value = None; Edges = [(None, "e");(None, "x")] |> Set.ofList })
                                    ] |> Map.ofList
                let placement = findAgentPlacement zone initialGraph
                Assert.AreEqual(4,List.length placement)
                ()
           
            [<Test>]
            member this.FindAgentPositions_1NodeGraph_Return1Positons() =
                let zone = [{ Identifier = "v23"; Value = Some 10; Edges = [(Some 8, "v16")] |> Set.ofList }]
                let initialGraph =  [ ("v23", { Identifier = "v23"; Value = Some 10; Edges = [(Some 8, "v16")] |> Set.ofList }) 
                                    ] |> Map.ofList
                let placement = findAgentPlacement zone initialGraph
                Assert.AreEqual(1,List.length placement)
                ()

            [<Test>]
            member this.FindZone_TwoNodes_LocateZone() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(None, "b")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "a" Explorer initialGraph
                let (Some (_,_,goals)) = findNewZone state
                let plan = makePlan state goals
                Assert.IsTrue (plan.IsSome)
                ()

            //           
                //     A---B---C---D---E
                //     |   |   |   |   |       
                //     F---G---H---I---J
                //             |    
                //     K---L---M---N---O 
                //     |   |   |   |   |  
                //     P---Q---R---S---T
                //        
            [<Test>]
            member this.FindZone_LotsOfNodes_LocateZone() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 1; Edges = [(None, "b");(None, "f")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = Some 1; Edges = [(None, "a");(None, "c");(None, "g")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = Some 1; Edges = [(None, "b");(None, "h");(None, "d")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = Some 1; Edges = [(None, "c");(None, "i");(None, "e")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = Some 1; Edges = [(None, "d");(None, "j")] |> Set.ofList })

                                    ; ("f", { Identifier = "f"; Value = Some 1; Edges = [(None, "a");(None, "g")] |> Set.ofList }) 
                                    ; ("g", { Identifier = "g"; Value = Some 1; Edges = [(None, "f");(None, "b");(None, "h")] |> Set.ofList })
                                    ; ("h", { Identifier = "h"; Value = Some 1; Edges = [(None, "g");(None, "c");(None, "i");(None, "m")] |> Set.ofList })
                                    ; ("i", { Identifier = "i"; Value = Some 1; Edges = [(None, "h");(None, "d");(None, "j")] |> Set.ofList })
                                    ; ("j", { Identifier = "j"; Value = Some 1; Edges = [(None, "i");(None, "e")] |> Set.ofList })

                                    ; ("k", { Identifier = "k"; Value = Some 1; Edges = [(None, "l");(None, "p")] |> Set.ofList }) 
                                    ; ("l", { Identifier = "l"; Value = Some 1; Edges = [(None, "k");(None, "m");(None, "q")] |> Set.ofList })
                                    ; ("m", { Identifier = "m"; Value = None; Edges = [(None, "h");(None, "l");(None, "n");(None, "r")] |> Set.ofList })
                                    ; ("n", { Identifier = "n"; Value = Some 8; Edges = [(None, "m");(None, "o");(None, "s")] |> Set.ofList })
                                    ; ("o", { Identifier = "o"; Value = Some 9; Edges = [(None, "n");(None, "t")] |> Set.ofList })

                                    ; ("p", { Identifier = "p"; Value = Some 1; Edges = [(None, "k");(None, "q")] |> Set.ofList }) 
                                    ; ("q", { Identifier = "q"; Value = None; Edges = [(None, "l");(None, "p");(None, "r")] |> Set.ofList })
                                    ; ("r", { Identifier = "r"; Value = Some 8; Edges = [(None, "m");(None, "q");(None, "s")] |> Set.ofList })
                                    ; ("s", { Identifier = "s"; Value = Some 9; Edges = [(None, "n");(None, "r");(None, "t")] |> Set.ofList })
                                    ; ("t", { Identifier = "t"; Value = Some 10; Edges = [(None, "o");(None, "s")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "t" Explorer initialGraph
                let (Some (_,_,goals)) = findNewZone state
                let plan = makePlan state [goals.Head]
                Assert.IsTrue (plan.IsSome)
                ()

            //           
                //     A---B---C---D---E
                //     |   |   |   |   |       
                //     F---G---H---I---J
                //             |    
                //     K---L---M---N---O 
                //     |   |   |   |   |  
                //     P---Q---R---S---T
                //        
            [<Test>]
            member this.CommunicateZone_LotsOfNodes_CorrectZoneCommunicated() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 1; Edges = [(None, "b");(None, "f")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = Some 1; Edges = [(None, "a");(None, "c");(None, "g")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = Some 1; Edges = [(None, "b");(None, "h");(None, "d")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = Some 1; Edges = [(None, "c");(None, "i");(None, "e")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = Some 1; Edges = [(None, "d");(None, "j")] |> Set.ofList })

                                    ; ("f", { Identifier = "f"; Value = Some 1; Edges = [(None, "a");(None, "g")] |> Set.ofList }) 
                                    ; ("g", { Identifier = "g"; Value = Some 1; Edges = [(None, "f");(None, "b");(None, "h")] |> Set.ofList })
                                    ; ("h", { Identifier = "h"; Value = Some 1; Edges = [(None, "g");(None, "c");(None, "i");(None, "m")] |> Set.ofList })
                                    ; ("i", { Identifier = "i"; Value = Some 1; Edges = [(None, "h");(None, "d");(None, "j")] |> Set.ofList })
                                    ; ("j", { Identifier = "j"; Value = Some 1; Edges = [(None, "i");(None, "e")] |> Set.ofList })

                                    ; ("k", { Identifier = "k"; Value = Some 1; Edges = [(None, "l");(None, "p")] |> Set.ofList }) 
                                    ; ("l", { Identifier = "l"; Value = Some 1; Edges = [(None, "k");(None, "m");(None, "q")] |> Set.ofList })
                                    ; ("m", { Identifier = "m"; Value = Some 5; Edges = [(None, "h");(None, "l");(None, "n");(None, "r")] |> Set.ofList })
                                    ; ("n", { Identifier = "n"; Value = Some 8; Edges = [(None, "m");(None, "o");(None, "s")] |> Set.ofList })
                                    ; ("o", { Identifier = "o"; Value = Some 9; Edges = [(None, "n");(None, "t")] |> Set.ofList })

                                    ; ("p", { Identifier = "p"; Value = Some 1; Edges = [(None, "k");(None, "q")] |> Set.ofList }) 
                                    ; ("q", { Identifier = "q"; Value = Some 5; Edges = [(None, "l");(None, "p");(None, "r")] |> Set.ofList })
                                    ; ("r", { Identifier = "r"; Value = Some 8; Edges = [(None, "m");(None, "q");(None, "s")] |> Set.ofList })
                                    ; ("s", { Identifier = "s"; Value = Some 9; Edges = [(None, "n");(None, "r");(None, "t")] |> Set.ofList })
                                    ; ("t", { Identifier = "t"; Value = Some 10; Edges = [(None, "o");(None, "s")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "t" Explorer initialGraph
                let (Some (_,_,goals)) = findNewZone state
                let plan = makePlan state goals
                let (_,newgoals) = plan.Value
                let newplan = makePlan state newgoals.Tail
                Assert.IsTrue ((fst newplan.Value).Head.ActionType = Communicate(CreateJob((None,44,JobType.OccupyJob,3),OccupyJob(["n";"r";"t"],["n";"o";"r";"s";"t"]))))
                ()

                //     K---L---M---N---O 
                //     |   |   |   |   |  
                //     P---Q---R---S---T
                //        
            [<Test>]
            member this.BonusFindZone_LotsOfNoneNodes_LocateZone() =
                let initialGraph =  [ ("k", { Identifier = "k"; Value = None; Edges = [(None, "l");(None, "p")] |> Set.ofList }) 
                                    ; ("l", { Identifier = "l"; Value = None; Edges = [(None, "k");(None, "m");(None, "q")] |> Set.ofList })
                                    ; ("m", { Identifier = "m"; Value = None; Edges = [(None, "l");(None, "n");(None, "r")] |> Set.ofList })
                                    ; ("n", { Identifier = "n"; Value = None; Edges = [(None, "m");(None, "o");(None, "s")] |> Set.ofList })
                                    ; ("o", { Identifier = "o"; Value = None; Edges = [(None, "n");(None, "t")] |> Set.ofList })

                                    ; ("p", { Identifier = "p"; Value = None; Edges = [(None, "k");(None, "q")] |> Set.ofList }) 
                                    ; ("q", { Identifier = "q"; Value = None; Edges = [(None, "l");(None, "p");(None, "r")] |> Set.ofList })
                                    ; ("r", { Identifier = "r"; Value = None; Edges = [(None, "m");(None, "q");(None, "s")] |> Set.ofList })
                                    ; ("s", { Identifier = "s"; Value = None; Edges = [(None, "n");(None, "r");(None, "t")] |> Set.ofList })
                                    ; ("t", { Identifier = "t"; Value = Some 10; Edges = [(None, "o");(None, "s")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "t" Explorer initialGraph
                let (Some (_,_,goals)) = findNewZone state
                let plan = makePlan state goals
                Assert.IsTrue (plan.IsSome)
                ()

                //     B---C
                //     |\ /|
                //     | A |
                //     |/ \|  
                //     E---D  
            [<Test>]
            member this.BonusFindZone2_LotsOfNoneNodesInASquare_LocateZone() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(Some 1, "b");(Some 1, "c");(Some 1, "d");(Some 1, "e")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 1, "a");(Some 1, "c");(Some 1, "e")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(Some 1, "a");(Some 1, "b");(Some 1, "d")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(Some 1, "a");(Some 1, "c");(Some 1, "e")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(Some 1, "a");(Some 1, "b");(Some 1, "d")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "a" Explorer initialGraph
               
                let (Some (_,_,goals)) = findNewZone state
                let plan = makePlan state (goals)

                Assert.IsTrue (plan.IsSome)
                ()

                //     B---C
                //      \ /
                //       A 
            [<Test>]
            member this.SmallTest_FewNodes_LocateZone() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(Some 1, "b");(Some 1, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 1, "a");(Some 1, "c")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(Some 1, "b");(Some 1, "a")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "a" Explorer initialGraph
                
                let (Some (_,_,goal::goals)) = findNewZone state
                let plan = makePlan state goals
                Assert.IsTrue (plan.IsSome)
                ()

            [<Test>]
            member this.MergeZoneTest_TwoZones_OneMergedZone() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(Some 1, "b");(Some 1, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = Some 8; Edges = [(Some 1, "a");(Some 1, "c")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = Some 8; Edges = [(Some 1, "b");(Some 1, "a");(Some 1, "d")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = Some 10; Edges = [(Some 1, "c")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "a" Explorer initialGraph
                
                let zone1 = ["a";"b";"c"]
                let zone2 = ["d";"c";"b"]
                
                let overlap = getOverlappingVertices zone1 zone2
                Assert.IsTrue(overlap.Length = 2)

                let overlapJob = ((Some 1,8,JobType.OccupyJob,2),OccupyJob(["a";"b"],["a";"b";"c"]))

                let merged = removeDuplicates <| mergeZones zone2 [overlapJob]
                Assert.IsTrue (merged.Length = 4)
                ()