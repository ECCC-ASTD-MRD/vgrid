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

#ifndef COAT_CHECK_H
#define COAT_CHECK_H

#include "vgrid.hpp"

// make this a Singleton.  See https://stackoverflow.com/questions/1008019/c-singleton-design-pattern
class coat_check
{
public:
  // Obtain the vgrid, given the coat-check tag
  vgrid_descriptor* get_vgrid(int tag);

  // Obtain a coat-check tag, given a vgrid
  int get_tag(vgrid_descriptor* vgrid);

  // Decrement the usage count on a particular vgrid
  void release_vgrid(int tag);

  // Debugging Instrumentation:  return the number of tags issued for this grid
  int grid_count(int tag);

private:
  // A coat_hanger holds all data relevant to one vertical grid
  struct coat_hanger{
    vgrid_descriptor *vgrid;
  };

  // The coat_closet hold all of the coat_hanger's (vgrids) together
  coat_hanger coat_closet[100];

};

#endif // COAT_CHECK_H
