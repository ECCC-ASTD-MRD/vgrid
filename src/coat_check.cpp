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
#include "vgrid.h"
#include <stdio.h>


coat_check::coat_check()
{    
  latest_hanger_filled=-1;
}

// Obtain the vgrid, given the coat-check tag
vgrid_descriptor* coat_check::get_vgrid(int tag)
{
  return &coat_closet[tag].vgrid;
  // Don't change num_tags_issued, because the client is still using the tag.
};


// Obtain a coat-check tag, given a vgrid
int coat_check::get_tag(vgrid_descriptor* vgrid)
{
  coat_hanger *hanger_p;
  int hanger_index;

  // Search the occupied hangers for the submitted vgrid
  for(hanger_index=0; hanger_index <= latest_hanger_filled; hanger_index++)
    {
      // TBD:  create and use vgrid operator ==
      if(vgrid::Cvgd_vgdcmp(&coat_closet[hanger_index].vgrid, vgrid) == 0)
        break;
    }

  if(hanger_index <= latest_hanger_filled)
    {
      // An identical vgrid is already in coat_closet at hanger_index
      coat_closet[hanger_index].num_tags_issued++;
      return hanger_index;
    }
  else
    {
      // Create a local copy of the submitted vgrid, in the coat_closet
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
          hanger_p=&coat_closet[++latest_hanger_filled];
          hanger_p->vgrid=*vgrid;
          hanger_p->num_tags_issued=1;
          return latest_hanger_filled;
        }

    }
};

// Decrement the usage count on a particular vgrid
void coat_check::release_vgrid(int tag)
{
  if(--coat_closet[tag].num_tags_issued < 0)
    coat_closet[tag].num_tags_issued = 0;
  // if(coat_closet[tag].num_tags_issued == 0)
  //   could do something, like delete vgrid
};

// Debugging Instrumentation:  return the number of tags issued for this grid
int coat_check::grid_count(int tag)
{
  return coat_closet[tag].num_tags_issued;
};
