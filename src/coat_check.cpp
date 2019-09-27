// libdescrip - Vertical grid descriptor library for FORTRAN programming
// Copyright (C) 2016  Direction du developpement des previsions nationales
//                     Centre meteorologique canadien
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "coat_check.h"

// Obtain the vgrid, given the coat-check tag
vgrid_descriptor* coat_check::get_vgrid(int tag)
{
  tag=1;
};

// Obtain a coat-check tag, given a vgrid
int coat_check::get_tag(vgrid_descriptor* vgrid)
{
//  ==============
//   Q:  Should the coat_check take a physical copy of vgrid, or just a pointer?

//   A:  It must be a copy, because I expect to have multiple clients using the
//       same vgrid.
//  ==============
  return 1;
};

// Obtain a coat-check tag, given a vgrid
void coat_check::release_vgrid(int tag)
{
};

// Debugging Instrumentation:  return the number of tags issued for this grid
int coat_check::grid_count(int tag)
{
};
