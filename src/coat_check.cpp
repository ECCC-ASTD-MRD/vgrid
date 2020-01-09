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

#include "coat_check.hpp"
#include "vgrid.hpp"
#include <stdio.h>

// COAT_CHECK METHODS:
// COAT_CHECK METHODS:

// Constructor
coat_check::coat_check()
{    
  latest_hanger_filled=-1;
}

// Obtain the vgrid, given the coat-check tag
vgrid* coat_check::get_vgrid(int tag)
{
  if(tag < 0 || tag > latest_hanger_filled)
    {
      printf("ERROR in coat_check:  invalid tag=%d\n",tag);
      throw tag;
    }
  return hangers[tag].vgd;
  // Don't change num_tags_issued, because the client is still using the tag.
};


// Obtain a coat-check tag, given a vgrid
int coat_check::get_tag(vgrid *vgrid_p)
 {
  coat_hanger *hanger_p;
  int hanger_index;

  // Search the occupied hangers for the submitted vgrid
  for(hanger_index=0; hanger_index <= latest_hanger_filled; hanger_index++)
    {
      // TBD:  create and use vgrid operator ==
      if(vgrid_p->Cvgd_vgdcmp(hangers[hanger_index].vgd) == 0)
        break;
    }

  if(hanger_index <= latest_hanger_filled)
    {
      // An identical vgrid is already in hangers at hanger_index
      hangers[hanger_index].num_tags_issued++;
      return hanger_index;
    }
  else
    {
      // Create a local copy of the submitted vgrid, in the hangers
      //    (This is necessary, because one can expect to have multiple clients
      //     using the same vgrid.  If one client deletes his vgrid, a copy must
      //     be available for the other clients.)
      if(latest_hanger_filled >= NUM_HANGERS-1)
        {
          printf("ERROR:  The number of vgrids has reached its maximum: %d\n",
                 NUM_HANGERS);
          return -999;
        }
      else
        {
          hanger_p=&hangers[++latest_hanger_filled];
          hanger_p->vgd=vgrid_p;
          hanger_p->num_tags_issued=1;
          return latest_hanger_filled;
        }

    }
};

// Decrement the usage count on a particular vgrid
void coat_check::release_vgrid(int tag)
{
  if(--hangers[tag].num_tags_issued < 0)
    hangers[tag].num_tags_issued = 0;
  // if(hangers[tag].num_tags_issued == 0)
  //   could do something, like delete vgrid
};

// Debugging Instrumentation:  return the number of tags issued for this grid
int coat_check::grid_count(int tag)
{
  return hangers[tag].num_tags_issued;
};


// COAT_HANGER METHODS:
// COAT_HANGER METHODS:

// Constructor
coat_hanger::coat_hanger()
{
  num_tags_issued = 0;
};
