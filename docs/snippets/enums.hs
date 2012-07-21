data Role = CEO
          | Manager
          | Developer
          | Marketing

data Customer = Customer
  { cid     :: Double
  , address :: String
  , role    :: Role
  }

monthlySalary :: Customer -> Double
monthlySalary customer =
  case role customer of
    CEO -> 1000000
    Manager -> 100000
    Marketing -> 1000
    Developer -> 1
