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

#include "vgrid_descriptor.h"

// make this a Singleton.  See https://stackoverflow.com/questions/1008019/c-singleton-design-pattern
class coat_check
// Context:  
//           While this class does not check coats, its name is a metaphor for
//           what it does with vgrid objects.
//
//           Coat_check is a tool that supports the interface to fortran.
//           Because it is not a good idea to inject C objects into fortran
//           code, coat_check allows the program to check those objects at the
//           interface to fortran, and to obtain an integer tag, or handle, that
//           the fortran code can use to refer to the C object, a vgrid in this
//           case.  When the fortan code calls a C routine, with the tag as an
//           argument, the C routine can go to the coat check with that tag and
//           obtain the C objectin question.
//
// Purpose:
//           To accept a vgrid object in exchange for a tag and, conversely, to
//           provide the indicated vgrid object when a tag is presented.
{
public:
  // Constructor
  coat_check(void);

  // Obtain the vgrid, given the coat-check tag
  vgrid_descriptor* get_vgrid(int tag);

  // Obtain a coat-check tag, given a vgrid
  int get_tag(vgrid_descriptor* vgrid);

  // Decrement the usage count on a particular vgrid
  void release_vgrid(int tag);

  // Debugging Instrumentation:  return the number of tags outstanding for this
  // grid
  int grid_count(int tag);

private:
  // A coat_hanger holds all data relevant to one vertical grid
  struct coat_hanger{
    vgrid_descriptor vgrid;
    int num_tags_issued;
  };

  // The coat_closet holds all of the coat_hangers (vgrids) together
  static const int NUM_HANGERS=100;
  coat_hanger coat_closet[NUM_HANGERS];

  int latest_hanger_filled;
};

#endif // COAT_CHECK_H
