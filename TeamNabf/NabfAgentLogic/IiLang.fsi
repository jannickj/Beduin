namespace IiLang
    module IiLangHandler =
        open JSLibrary.IiLang
        open JSLibrary.IiLang.Parameters
        open JSLibrary.IiLang.DataContainers
        open System.Xml.Serialization

        type Element = ParameterList of Element list
                     | Function      of (string * Element list)
                     | Numeral       of float
                     | Identifier    of string

        type Data = Percept of (string * Element list)
                  | Action  of (string * Element list)

        type PerceptCollection = Data list

        val parsePercept : IilPercept -> Data