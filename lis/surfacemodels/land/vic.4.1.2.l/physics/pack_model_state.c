#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vicNl.h>

static char vcid[] = "$Id$";
void pack_data(float *states, int *count, float val)
{
    states[*count] = val;
    *count += 1;
}

void pack_model_state(dist_prcp_struct    *prcp,
                      global_param_struct *gp,
                      int                  Nveg,
                      int                  cellnum,
                      filep_struct        *filep,
                      soil_con_struct     *soil_con,
                      char                *STILL_STORM,
                      int                 *DRY_TIME,
                      float               *states,
                      int                 *count)
{
    extern option_struct options;

    double tmpval;
    int    veg;
    int    band;
    int    lidx;
    int    nidx;
    int    dist;
    int    Ndist;
    int    Nbands;
    int    byte;

    *count = 0;
    options.BINARY_STATE_FILE = TRUE;

#if SPATIAL_FROST
    int    frost_area;
#endif // SPATIAL_FROST

    cell_data_struct     ***cell;
    snow_data_struct      **snow;
    energy_bal_struct     **energy;
    veg_var_struct       ***veg_var;
    lake_var_struct         lake_var;
    int    node;

    if(options.DIST_PRCP)
        Ndist = 2;
    else
        Ndist = 1;
    Nbands = options.SNOW_BAND;

    cell    = prcp->cell;
    veg_var = prcp->veg_var;
    snow    = prcp->snow;
    energy  = prcp->energy;
    lake_var = prcp->lake_var;

    /* write cell information */
    pack_data(states, count, 1.0*cellnum);
    pack_data(states, count, 1.0*Nveg);
    pack_data(states, count, 1.0*Nbands);

    /* Write soil thermal node deltas */
    for ( nidx = 0; nidx < options.Nnode; nidx++ )
    {
        pack_data(states, count, 1.0*soil_con->dz_node[nidx]);
    }

    /* Write soil thermal node depths */
    for ( nidx = 0; nidx < options.Nnode; nidx++ )
    {
        pack_data(states, count, 1.0*soil_con->Zsum_node[nidx]);
    }

    /* Write dynamic soil properties */
#if EXCESS_ICE
    /* Write soil depth */
    for ( lidx = 0; lidx < options.Nlayer; lidx++ )
    {
        pack_data(states, count, 1.0*soil_con->depth[lidx]);
    }

    /* Write effective porosity */
    for ( lidx = 0; lidx < options.Nlayer; lidx++ )
    {
        pack_data(states, count, 1.0*soil_con->effective_porosity[lidx]);
    }

    /* Write damping depth */
    pack_data(states, count, 1.0*soil_con->dp);
#endif

    /* Output for all vegetation types */
    for ( veg = 0; veg <= Nveg; veg++ )
    {
        // Store distributed precipitation fraction
        pack_data(states, count, 1.0*prcp->mu[veg]);

        // Store distributed precipitation variables
        //pack_data(states, count, 1.0*STILL_STORM[veg]);
        if(STILL_STORM[veg]==TRUE)
            pack_data(states, count, 1.0);
        else
            pack_data(states, count, 0.0);

        pack_data(states, count, 1.0*DRY_TIME[veg]);

        /* Output for all snow bands */
        for ( band = 0; band < Nbands; band++ )
        {
            /* Write cell identification information */
            pack_data(states, count, 1.0*veg);
            pack_data(states, count, 1.0*band);

            for ( dist = 0; dist < Ndist; dist ++ )
            {
                // Store both wet and dry fractions if using distributed precipitation
                /* Write total soil moisture */
                for ( lidx = 0; lidx < options.Nlayer; lidx++ )
                {
                    tmpval = cell[dist][veg][band].layer[lidx].moist;
                    pack_data(states, count, 1.0*tmpval);
                }

                /* Write average ice content */
                for ( lidx = 0; lidx < options.Nlayer; lidx++ )
                {
#if SPATIAL_FROST
                    for ( frost_area = 0; frost_area < FROST_SUBAREAS; frost_area++ )
                    {
                        tmpval = cell[dist][veg][band].layer[lidx].ice[frost_area];
                        pack_data(states, count, 1.0*tmpval);
                    }
#else
                    tmpval = cell[dist][veg][band].layer[lidx].ice;
                    pack_data(states, count, 1.0*tmpval);
#endif // SPATIAL_FROST
                }

                /* Write dew storage */
                if ( veg < Nveg )
                {
                    tmpval = veg_var[dist][veg][band].Wdew;
                    pack_data(states, count, 1.0*tmpval);
                }
            }

            /* Write snow data */
            pack_data(states, count, 1.0*snow[veg][band].last_snow);
            pack_data(states, count, 1.0*snow[veg][band].MELTING);
            pack_data(states, count, 1.0*snow[veg][band].coverage);
            pack_data(states, count, 1.0*snow[veg][band].swq);
            pack_data(states, count, 1.0*snow[veg][band].surf_temp);
            pack_data(states, count, 1.0*snow[veg][band].surf_water);
            pack_data(states, count, 1.0*snow[veg][band].pack_temp);
            pack_data(states, count, 1.0*snow[veg][band].pack_water);
            pack_data(states, count, 1.0*snow[veg][band].density);
            pack_data(states, count, 1.0*snow[veg][band].coldcontent);
            pack_data(states, count, 1.0*snow[veg][band].snow_canopy);

            /* Write soil thermal node temperatures */
            for ( nidx = 0; nidx < options.Nnode; nidx++ )
            {
                pack_data(states, count, 1.0*energy[veg][band].T[nidx]);
            }

        }
    }

    if ( options.LAKES )
    {
        for ( dist = 0; dist < Ndist; dist ++ )
        {
            // Store both wet and dry fractions if using distributed precipitation
            /* Write total soil moisture */
            for ( lidx = 0; lidx < options.Nlayer; lidx++ )
            {
                pack_data(states, count, 1.0*lake_var.soil.layer[lidx].moist);
            }

            /* Write average ice content */
            for ( lidx = 0; lidx < options.Nlayer; lidx++ )
            {
#if SPATIAL_FROST
                for ( frost_area = 0; frost_area < FROST_SUBAREAS; frost_area++ )
                {
                    pack_data(states, count, 1.0*lake_var.soil.layer[lidx].ice[frost_area]);
                }
#else
                pack_data(states, count, 1.0*lake_var.soil.layer[lidx].ice);
#endif // SPATIAL_FROST
            }
        }
        /* Write snow data */
        pack_data(states, count, 1.0*lake_var.snow.last_snow);
        pack_data(states, count, 1.0*lake_var.snow.MELTING);
        pack_data(states, count, 1.0*lake_var.snow.coverage);
        pack_data(states, count, 1.0*lake_var.snow.swq);
        pack_data(states, count, 1.0*lake_var.snow.surf_temp);
        pack_data(states, count, 1.0*lake_var.snow.surf_water);
        pack_data(states, count, 1.0*lake_var.snow.pack_temp);
        pack_data(states, count, 1.0*lake_var.snow.pack_water);
        pack_data(states, count, 1.0*lake_var.snow.density);
        pack_data(states, count, 1.0*lake_var.snow.coldcontent);
        pack_data(states, count, 1.0*lake_var.snow.snow_canopy);

        /* Write soil thermal node temperatures */
        for ( nidx = 0; nidx < options.Nnode; nidx++ )
        {
            pack_data(states, count, 1.0*lake_var.energy.T[nidx]);
        }

        /* Write lake-specific variables */
        pack_data(states, count, 1.0*lake_var.activenod);
        pack_data(states, count, 1.0*lake_var.dz);
        pack_data(states, count, 1.0*lake_var.surfdz);
        pack_data(states, count, 1.0*lake_var.ldepth);

        for ( node = 0; node <= lake_var.activenod; node++ )
        {
            pack_data(states, count, 1.0*lake_var.surface[node]);
        }

        pack_data(states, count, 1.0*lake_var.sarea);
        pack_data(states, count, 1.0*lake_var.volume);

        for ( node = 0; node < lake_var.activenod; node++ )
        {
            pack_data(states, count, 1.0*lake_var.temp[node]);
        }

        pack_data(states, count, 1.0*lake_var.tempavg);
        pack_data(states, count, 1.0*lake_var.areai);
        pack_data(states, count, 1.0*lake_var.new_ice_area);
        pack_data(states, count, 1.0*lake_var.ice_water_eq);
        pack_data(states, count, 1.0*lake_var.hice);
        pack_data(states, count, 1.0*lake_var.tempi);
        pack_data(states, count, 1.0*lake_var.swe);
        pack_data(states, count, 1.0*lake_var.surf_temp);
        pack_data(states, count, 1.0*lake_var.pack_temp);
        pack_data(states, count, 1.0*lake_var.coldcontent);
        pack_data(states, count, 1.0*lake_var.surf_water);
        pack_data(states, count, 1.0*lake_var.pack_water);
        pack_data(states, count, 1.0*lake_var.SAlbedo);
        pack_data(states, count, 1.0*lake_var.sdepth);
    }
}
