namespace NabfAgentLogic.Search
module FloydWarshall =
    
    open Graphing.Graph

    let rec addEdgesToMap (heuristicMap:Map<VertexName*VertexName,int>) (origin:VertexName) (edges:DirectedEdge list) =
        match edges with
        | (value,name) :: tail ->
                let intValue = if value.IsSome then value.Value else 10
                let newMap = heuristicMap.Add((origin,name),intValue).Add((name,origin),intValue)
                addEdgesToMap newMap origin tail
        | [] -> heuristicMap

    let rec removeDuplicates (zone:VertexName list) (result:VertexName list) =
        match zone with
        | head :: tail -> 
                        let target = List.tryFind (fun n -> n = head) tail
                        if target.IsSome then removeDuplicates tail result else removeDuplicates tail (head::result)
        | [] -> result

    let rec generateMap (names:VertexName list) (i:int) (map:Map<int,string>) =
        match names with
        | head :: tail -> generateMap tail (i+1) (map.Add(i,head))
        | [] -> map

    let floydWarshall (map:Graph) (heuristicMap:Map<VertexName*VertexName,int>) (origin:VertexName) =
        let names = List.sort (removeDuplicates (List.append [origin] (List.map fst (Map.toList map))) [])
        let nameMap = generateMap names 0 Map.empty
        let n = names.Length
        let mapWithSelf = heuristicMap.Add ((origin,origin),0)
        let edgeList = Set.toList map.[origin].Edges
        let mapWithNeighbours = addEdgesToMap mapWithSelf origin edgeList
        let mutable hMap = mapWithNeighbours
        for k in 0 .. (n-1) do
            for j in 0 .. (n-1) do
                let val1 = Map.tryFind (origin,nameMap.[k]) hMap//hMap.[origin,nameMap.[k]]
                let val2 = Map.tryFind (nameMap.[k],nameMap.[j]) hMap//hMap.[nameMap.[k],nameMap.[j]]
                let val3 = Map.tryFind (origin,nameMap.[j]) hMap
                let v = match val1 with
                                   |Some x -> match val2 with
                                              |Some y -> x + y
                                              |None -> 999
                                   |None -> 999
                if val3.IsNone || (val3.IsSome && hMap.[origin,nameMap.[j]] > v) 
                then
                    hMap <- hMap.Add((nameMap.[j],origin),v)
                    hMap <- hMap.Add((origin,nameMap.[j]),v)
        hMap

    let rec floydWarshallList (map:Graph) (heuristicMap:Map<VertexName*VertexName,int>) (origins:VertexName list) =
        match origins with
        | head :: tail -> floydWarshallList map (floydWarshall map heuristicMap head) tail
        | [] -> heuristicMap
    
    let floydWarshallSimple (map:Graph) =
        let names = List.sort (List.map fst (Map.toList map))
        let nameMap = generateMap names 1 Map.empty
        let n = names.Length
        
        let edgeMap = ref Map.empty
        for i in 1 .. n do
            let vertex = map.[nameMap.[i]]
            
            Set.iter (fun e -> 
                            match e with
                            | (Some cost,name) -> edgeMap := Map.add (vertex.Identifier,name) cost !edgeMap
                            | (None,name) -> edgeMap := Map.add (vertex.Identifier,name) 1 !edgeMap
                            ) vertex.Edges
            ()
        let mutable hMap = !edgeMap
        for k in 1 .. n do
            for i in 1 .. n do
                for j in 1 .. n do
                    
                    let getValue map a b = 
                        match Map.tryFind (nameMap.[a],nameMap.[b]) map with
                        | Some value -> value
                        | None -> 9999999
                    
                    let valik = getValue hMap i k
                    let valkj = getValue hMap k j
                    let valij = getValue hMap i j                    
                    
                    if valij > valik + valkj
                    then
                        let newij = valik + valkj
                        hMap <- Map.add (nameMap.[i],nameMap.[j]) newij hMap
        hMap
    
    let floydWarshallComplete (map:Graph) =
        floydWarshallSimple map
        //let vertices = List.sort (removeDuplicates ((List.map fst (Map.toList map))) [])
        //floydWarshallList map Map.empty<VertexName*VertexName,int> vertices