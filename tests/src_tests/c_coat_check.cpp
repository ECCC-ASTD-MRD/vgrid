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

#include <stdio.h>
#include "vgrid.hpp"
#include "coat_check.h"
#include "c_ut_report.h"

extern "C" void c_coat_check() {
  int status, ier;
  int tag1, tag2, tag_a, tag_b;
  int grid_count_a, grid_count_b;

  coat_check my_coat_check;

  vgrid_descriptor my_vgrid_a, my_vgrid_b, *checked_vgrid_p, *my_vgrid_a_p;


  // Test 1:  If I put a second (different) grid in the coat check, I should get
  //          a tag that is different from the first one
  tag1 = my_coat_check.get_tag(&my_vgrid_a);
  tag2 = my_coat_check.get_tag(&my_vgrid_b);

  if(tag1 == tag2)
    {
      printf("Error:  two different grids received the same tag:  %d %d\n",
             tag1, tag2);
      status = VGD_ERROR;
    }

  // Save the tags for a later test
  tag_a = tag1;
  tag_b = tag2;


  // Test 2:  If I put a second identical grid in the coat check, I should get
  //          the same tag as the first one
  tag2 = my_coat_check.get_tag(&my_vgrid_a);

  if(tag1 != tag_a)
    {
      printf("Error:  two identical grids did not receive the same "
                     "tag:  %d %d\n", tag1, tag_a);
      status = VGD_ERROR;
    }

  // Test 3:  If I retrieve a grid from the coat check, it should be the same as
  //          the original grid (equal value)
  checked_vgrid_p = my_coat_check.get_vgrid(tag_a);

  my_vgrid_a_p = &my_vgrid_a;
  if(!(my_vgrid_a_p == checked_vgrid_p))
    {
      printf("Error:  get_vgrid does not give back the original vgrid\n");
      status = VGD_ERROR;
    }

  // Test 4:  Check that the grid counts are correct
  grid_count_a = my_coat_check.grid_count(tag_a);
  grid_count_b = my_coat_check.grid_count(tag_b);

  if(   grid_count_a != 2
     || grid_count_b != 1)
    {
      printf("Error:  grid counts are not as expected:  %d and %d\n",
             grid_count_a , grid_count_b);
      status = VGD_ERROR;
    }

  // Test 5:  Release_vgrid should reduce grid counts, but not beyond zero
  my_coat_check.release_vgrid(tag_a); // Release a vgrid_a
  grid_count_a = my_coat_check.grid_count(tag_a);
  if(grid_count_a != 1)
    {
      printf("Error:  release_vgrid yielded %d instead of 1\n", grid_count_a);
      status = VGD_ERROR;
    }

  my_coat_check.release_vgrid(tag_a); // Release a 2nd vgrid_a
  grid_count_a = my_coat_check.grid_count(tag_a);
  if(grid_count_a != 0)
    {
      printf("Error:  release_vgrid yielded %d instead of 0\n", grid_count_a);
      status = VGD_ERROR;
    }

  my_coat_check.release_vgrid(tag_a); // Release a vgrid_a that never existed
  grid_count_a = my_coat_check.grid_count(tag_a);
  if(grid_count_a != 0)
    {
      printf("Error:  release_vgrid when none are left yielded %d "
                     "instead of 0\n", grid_count_a);
      status = VGD_ERROR;
    }

  my_coat_check.release_vgrid(tag_b); // Release the vgrid_b
  grid_count_b = my_coat_check.grid_count(tag_b);
  if(grid_count_b != 0)
    {
      printf("Error:  release_vgrid yielded %d instead of 0\n", grid_count_b);
      status = VGD_ERROR;
    }

  ier = c_ut_report(status,"testing Cvgd_levels");
};
