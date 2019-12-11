module PersonForm exposing (PersonForm)


type PersonForm
    = PersonForm PersonFormFields


type alias PersonFormFields =
    { name : Alphabetical3To50Field
    , lastName : Alphabetical3To50Field
    , address : Alphabetical3To150Field
    , email : EmailField
    , phone : Numerical10Field
    , civilStatus : ChooseCivilStatusField
    , gender : ChooseGenderField
    , birthday : DateCenturyAgoToNow
    , focusField : Maybe Field
    }
