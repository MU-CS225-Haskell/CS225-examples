module Complex where

infix 6 :+
data Complex a = !a :+ !a
  deriving (Eq, Show, Read)

-- | Extracts real part of a complex number
re :: RealFloat a => Complex a -> a
re (a :+ _) = a

-- | Extracts imaginary part of a complex number
im :: RealFloat a => Complex a -> a
im (_ :+ b) = b

-- | Implementing numeric functions on Complex numbers
instance RealFloat a => Num (Complex a) where
  -- | Addition
  (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
  -- | Multiplication
  (a :+ b) * (c :+ d) = (a*c - b*d) :+ (b*c + a*d)
  -- | Negation
  negate (a :+ b) = negate a :+ negate b
  -- | Absolute value
  abs (a :+ b) = sqrt (a^2 + b^2) :+ 0
  -- | Signum
  signum   (0 :+ 0) = 0 :+ 0
  signum z@(a :+ b) = a/r :+ b/r where r = re (abs z)
  -- | Conversion from integers
  fromInteger n = fromInteger n :+ 0

magnitude :: RealFloat a => Complex a -> a
magnitude = re . abs

conjugate :: RealFloat a => Complex a -> Complex a
conjugate (a :+ b) = a :+ (-b)

-- | Fractional instances
instance (RealFloat a) => Fractional (Complex a) where
  (a :+ b) / z@(c :+ d) = (a*c + b*d)/k :+ (b*c - a*d)/k
                            where k = (magnitude z)^2
  fromRational a = fromRational a :+ 0
