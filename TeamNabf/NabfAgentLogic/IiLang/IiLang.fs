namespace NabfAgentLogic.IiLang
    module IiLangDefinitions =
        open JSLibrary.IiLang
        open JSLibrary.IiLang.Parameters
        open JSLibrary.IiLang.DataContainers
        open System.Xml.Serialization

        type Element = ParameterList of Element list
                     | Function      of (string * Element list)
                     | Numeral       of float
                     | Identifier    of string
        
        type Data = string * Element list
        
        type DataContainer = Percept of Data
                           | Action  of Data

        type PerceptCollection = Data list

        let rec buildIil (data : Element) : IilParameter = 
            let unpackParameters ls = List.map buildIil ls |> List.toSeq<IilParameter>
            match data with
            | ParameterList ls ->
                new IilParameterList (unpackParameters ls) 
                :> IilParameter
            | Function (name, ls) -> 
                new IilFunction (name, unpackParameters ls)
                :> IilParameter
            | Numeral num -> 
                new IilNumeral (num) 
                :> IilParameter
            | Identifier str -> 
                new IilIdentifier (str) 
                :> IilParameter

        let rec evalIil (iil : IilParameter) = 
            match iil with
            | :? Parameters.IilParameterList as paramlist 
                -> ParameterList (List.ofSeq paramlist.Parameters |> List.map evalIil)
            | :? Parameters.IilFunction as func           
                -> Function (func.Name, List.ofSeq func.Parameters |> List.map evalIil)
            | :? Parameters.IilIdentifier as id           
                -> Identifier id.Value
            | :? Parameters.IilNumeral as num             
                -> Numeral num.Value
            | _ -> failwith "the object %O is not a recognized iilang element (IilElement)"

        let parsePercept (iilPercept : IilPercept) = 
            Percept <| (iilPercept.Name, List.ofSeq iilPercept.Parameters |> List.map evalIil) 

        let parsePerceptCollection (iilPerceptCollection : IilPerceptCollection) =
            (List.ofSeq iilPerceptCollection.Percepts |> List.map parsePercept)

        let buildIilAction (action : DataContainer) : IilAction =
            match action with
            | Action (name, elements) -> 
                let parameters = (List.toSeq <| List.map buildIil elements)
                new IilAction (name, new System.Collections.Generic.LinkedList<IilParameter>(parameters))
            | _ -> failwith "wrong data type"