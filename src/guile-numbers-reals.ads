----------------------------------------------------------------------
--              G U I L E . N U M B E R S . R E A L S               --
--                                                                  --
--                    Interfacing to Guile reals                    --
--                                                                  --
--                             S p e c                              --
--                                                                  --
--   This file is part of GARBLE, the Guile/Ada Rich Binding for    --
--     Language Extension, a binding for Ada 2005 to Guile, the     --
--        GNU Ubiquitous Intelligent Language for Extension.        --
--                                                                  --
--                    Copyright © 2010 Atom X                       --
--                                                                  --
-- This library is free software; you can redistribute it and/or    --
-- modify it under the terms of the GNU General Public License as   --
-- published by the Free Software Foundation, version 3 of the      --
-- License, or (at your option) any later version.                  --
--                                                                  --
-- This program is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of   --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU --
-- General Public License for more details.                         --
--                                                                  --
-- You should have received a copy of the GNU General Public        --
-- License along with this library. If not, see                     --
-- <http://www.gnu.org/licenses/>                                   --
----------------------------------------------------------------------


--  <description>
--  Guile has rich support for expressing the real numbers, i.e. the
--  set of values of all possible points along a continuous, infinite,
--  one-dimensional line.  Guile can also represent rational numbers,
--  e.g. the real numbers that can be expressed as one integer divided
--  by another.
--
--  Guile also has the concept of exact vs. inexact representations of
--  numbers; rational numbers can be expressed either as decimal
--  numbers with a fractional part, e.g. 1.2, or as exact rationals,
--  i.e. 6/5.  Due to the limitations of computers, however, Guile is
--  unable to represent irrational numbers, such as π, with true
--  accuracy.
--
--  More information on Guile's real number abilities can be found in
--  its Reference Manual.  The reality for this library is that
--  Guile's real numbers will be converted back and forth between
--  Ada's Floats (specifically Long_Long_Float), and many of the
--  interesting and powerful abilities inherent in the Scheme language
--  will not be available directly to Ada code through this library.
--  They <em>could</em> be implemented, but this is currently beyond
--  the scope of this binding.
--  </description>
--  <c_version>1.9.11</c_version>

package Guile.Numbers.Reals is

   function Is_Real (X : SCM) return Boolean;
   --  Returns True if X is a real number, False otherwise.  Note that
   --  the sets of integer and rational numbers form subsets of the
   --  set of real numbers, so this predicate will return True if X
   --  represents an integer or a rational number as well.

   function Is_Rational (X : SCM) return Boolean;
   --  Returns True if x is a rational number, False otherwise.  Note
   --  that the set of integer values forms a subset of the set of
   --  rational numbers, thus this predicate will also return true if
   --  X is an integer.

   function Rationalize (X : SCM; Epsilon : SCM) return SCM;
   --  Returns the <em>simplest</em> rational number differing from X
   --  by no more than Epsilon.
   --
   --  As required by R5RS, &ldquo;rationalize&rdquo; only returns an
   --  exact result when both X and Epsilon are exact as well.  The
   --  use of Guile.Numbers.Exactness.Inexact_To_Exact may be required.

   function Is_Infinite (X : SCM) return Boolean;
   --  Returns True if X is either &ldquo;+inf.0&rdquo; or
   --  &ldquo;-inf.0&rdquo;, False otherwise.

   function Is_Not_A_Number (X : SCM) return Boolean;
   --  Returns True if X holds the not-a-number value
   --  &ldquo;+nan.0&rdquo;, False otherwise.

   function Nan return SCM;
   --  Returns the NaN value.

   function Infinity return SCM;
   --  Returns the &ldquo;infinity&rdquo; representation.

   function Numerator (X : SCM) return Long_Long_Integer;
   function Numerator (X : SCM) return SCM;
   --  Returns the numerator of the rational number X.

   function Denominator (X : SCM) return Long_Long_Integer;
   function Denominator (X : SCM) return SCM;
   --  Returns the denominator of the rational number X.

   function To_Ada (Val : SCM) return Long_Long_Float;
   --  Returns the number closest to Val that is representable as a
   --  Long_Long_Float.  Returns the IEEE value for infinity for any
   --  Val that is too large in magnitude to fit in a
   --  Long_Long_Float.  The argument Val must be a real number,
   --  i.e. Is_Real must return True.

   function To_Scheme (Val : Long_Long_Float) return SCM;
   --  Returns the SCM value that represents Val.  The value is
   --  inexact according to the predicate Is_Inexact, but it will be
   --  exactly equal to Val.

private

   pragma Import (C, To_Ada,      "scm_to_double");
   pragma Import (C, To_Scheme,   "scm_from_double");
   pragma Import (C, Nan,         "scm_nan");
   pragma Import (C, Infinity,    "scm_inf");
   pragma Import (C, Rationalize, "scm_rationalize");

end Guile.Numbers.Reals;
