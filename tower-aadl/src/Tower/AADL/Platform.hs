module Tower.AADL.Platform where

data HW =
    QEMU
  | ODROID
  | PIXHAWK
  deriving (Show, Read, Eq)

data OS =
    CAmkES
  | EChronos
  deriving (Show, Read, Eq)

