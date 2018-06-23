type Label = string
type VitalForce = { units: int }
type M<'LiveBodyPart> =
  M of (VitalForce -> 'LiveBodyPart * VitalForce)

let runM (M f) vitalForce = f vitalForce

let returnM x =
  M (fun vitalForce -> x, vitalForce)

let bindM f bodyPartM =
  let becomeAlive vitalForce =
    let bodyPart, remainingVitalForce = runM bodyPartM vitalForce
    runM (f bodyPart) remainingVitalForce
  M becomeAlive

type MonsterBuilder() =
  member __.Return(x) = returnM x
  member __.Bind(xM, f) = bindM f xM

let monster = new MonsterBuilder()

let mapM f xM =
  monster {
    let! x = xM
    return f x
  }

let map2M f xM yM =
  monster {
    let! x = xM
    let! y = yM
    return f x y
  }

let applyM fM xM =
  monster {
    let! f = fM
    let! x = xM
    return f x
  }

let getM =
  let doSomethingWhileLive vitalForce = vitalForce, vitalForce
  M doSomethingWhileLive

let putM newVitalForce =
  let doSomethingWhileLive _ = (), newVitalForce
  M doSomethingWhileLive

let getVitalForce vitalForce =
  let oneUnit = { units = 1 }
  let remaining = { units = vitalForce.units - 1 }
  oneUnit, remaining

let useUpOneUnitM =
  monster {
    let! vitalForce = getM
    let oneUnit, remainingVitalForce = getVitalForce vitalForce
    do! putM remainingVitalForce
    return oneUnit
  }

type DeadLeftLeg = DeadLeftLeg of Label
type LiveLeftLeg = LiveLeftLeg of Label * VitalForce
type DeadLeftBrokenArm = DeadLeftBrokenArm of Label
type LiveLeftBrokenArm = LiveLeftBrokenArm of Label * VitalForce
type LiveLeftArm = LiveLeftArm of Label * VitalForce
type DeadRightLowerArm = DeadRightLowerArm of Label
type DeadRightUpperArm = DeadRightUpperArm of Label
type LiveRightLowerArm = LiveRightLowerArm of Label * VitalForce
type LiveRightUpperArm = LiveRightUpperArm of Label * VitalForce
type LiveRightArm = {
  lowerArm : LiveRightLowerArm
  upperArm : LiveRightUpperArm
}
type DeadBrain = DeadBrain of Label
type Skull = Skull of Label
type LiveBrain = LiveBrain of Label * VitalForce
type LiveHead = {
  brain : LiveBrain
  skull : Skull
}
type DeadHeart = DeadHeart of Label
type LiveHeart = LiveHeart of Label * VitalForce
type BeatingHeart = BeatingHeart of LiveHeart * VitalForce
type LiveBody = {
  leftLeg  : LiveLeftLeg
  rightLeg : LiveLeftLeg
  leftArm  : LiveLeftArm
  rightArm : LiveRightArm
  head     : LiveHead
  heart    : BeatingHeart
}

let makeLiveLeftLegM (DeadLeftLeg label) =
  monster {
    let! oneUnit = useUpOneUnitM
    return LiveLeftLeg (label, oneUnit)
  }

let healBrokenArm (LiveLeftBrokenArm (label, vf)) =
  LiveLeftArm(label, vf)

let healBrokenArmM = mapM healBrokenArm

let makeLiveRightLowerArm (DeadRightLowerArm label) =
  monster {
    let! oneUnit = useUpOneUnitM
    return LiveRightLowerArm (label, oneUnit)
  }

let makeLiveRightUpperArm (DeadRightUpperArm label) =
  monster {
    let! oneUnit = useUpOneUnitM
    return LiveRightUpperArm (label, oneUnit)
  }

let rightArmM = monster {
  let! lowerArm = DeadRightLowerArm "Tom" |> makeLiveRightLowerArm
  let! upperArm = DeadRightUpperArm "Jerry" |> makeLiveRightUpperArm
  return { lowerArm = lowerArm; upperArm = upperArm; }
}

let makeLiveHeart (DeadHeart label) =
  monster {
    let! oneUnit = useUpOneUnitM
    return LiveHeart (label, oneUnit)
  }

let makeBeatingHeart liveHeart =
  monster {
    let! oneUnit = useUpOneUnitM
    return BeatingHeart (liveHeart, oneUnit)
  }

let headSurgery brain skull =
  { brain = brain; skull = skull }

let makeLiveBrain (DeadBrain label) =
  monster {
    let! oneUnit = useUpOneUnitM
    return LiveBrain (label, oneUnit)
  }

let makeBeatingHeartFromLiveHeartM liveHeartM =
  monster {
    let! liveHeart = liveHeartM
    let! beatingHeart = makeBeatingHeart liveHeart
    return beatingHeart
  }

let leftLegM = DeadLeftLeg "Boris" |> makeLiveLeftLegM

let makeLiveLeftBrokenArm (DeadLeftBrokenArm label) =
  monster {
    let! oneUnit = useUpOneUnitM
    return LiveLeftBrokenArm (label, oneUnit)
  }

let leftArmM =
  DeadLeftBrokenArm "Victor"
  |> makeLiveLeftBrokenArm
  |> mapM healBrokenArm

let headM =
  let brain = DeadBrain "Abby Normal" |> makeLiveBrain
  let skull = Skull "Yorick" |> returnM
  map2M headSurgery brain skull

let beatingHeartM =
  DeadHeart "Anne"
  |> makeLiveHeart
  |> bindM makeBeatingHeart

let createBody leftLeg rightLeg leftArm rightArm head beatingHeart =
  {
    leftLeg  = leftLeg
    rightLeg = rightLeg
    leftArm  = leftArm
    rightArm = rightArm
    head     = head
    heart    = beatingHeart
  }

let (<*>) = applyM
let (<!>) = mapM

let bodyM =
  createBody
  <!> leftLegM
  <*> leftLegM
  <*> leftArmM
  <*> rightArmM
  <*> headM
  <*> beatingHeartM

let vf = { units = 10 }
let liveBody, remainingFromBody = runM bodyM vf
printfn "%A %A" liveBody remainingFromBody
