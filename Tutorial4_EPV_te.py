#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#%%
"""
Created on Mon Apr 13 11:34:26 2020

Script for tutorial 4 of "Friends of Tracking" #FoT on assessing player passing options using expected possession value (EPV) and pitch control

Data can be found at: https://github.com/metrica-sports/sample-data

Accompanying video tutorials can be found here: https://www.youtube.com/watch?v=KXSLKwADXKI

GitHub repo: https://github.com/Friends-of-Tracking-Data-FoTD/LaurieOnTracking

@author: Laurie Shaw (@EightyFivePoint)
"""

import Metrica_IO as mio
import Metrica_Viz as mviz
import Metrica_Velocities as mvel
import Metrica_PitchControl as mpc
import Metrica_EPV as mepv

# set up initial path to data
DATADIR = 'data'
game_id = 2  # let's look at sample match 2

#%%
# read in the event data
events = mio.read_event_data(DATADIR, game_id)

#%%
# read in tracking data
tracking_home = mio.tracking_data(DATADIR, game_id, 'Home')
tracking_away = mio.tracking_data(DATADIR, game_id, 'Away')

#%%
# Convert positions from metrica units to meters (note change in Metrica's coordinate system since the last lesson)
tracking_home = mio.to_metric_coordinates(tracking_home)
tracking_away = mio.to_metric_coordinates(tracking_away)
events = mio.to_metric_coordinates(events)

#%%
# reverse direction of play in the second half so that home team is always attacking from right->left
tracking_home, tracking_away, events = mio.to_single_playing_direction(
    tracking_home, tracking_away, events
)

#%%
# Calculate player velocities
# tracking_home = mvel.calc_player_velocities(tracking_home, smoothing=True)
# tracking_away = mvel.calc_player_velocities(tracking_away, smoothing=True)
#%%
# **** NOTE *****
# if the lines above produce an error (happens for one version of numpy) change them to the lines below:
# ***************
tracking_home = mvel.calc_player_velocities(
    tracking_home, smoothing=True, filter_='moving_average'
)
tracking_away = mvel.calc_player_velocities(
    tracking_away, smoothing=True, filter_='moving_average'
)
""" *** UPDATES TO THE MODEL: OFFSIDES """
# first get pitch control model parameters
params = mpc.default_model_params()
# find goalkeepers for offside calculation
GK_numbers = [
    mio.find_goalkeeper(tracking_home),
    mio.find_goalkeeper(tracking_away)
]
#%%
""" *** GET EPV SURFACE **** """
home_attack_direction = mio.find_playing_direction(
    tracking_home, 'Home'
)  # 1 if shooting left-right, else -1

#%%
EPV = mepv.load_EPV_grid(DATADIR + '/EPV_grid.csv')
EPV
#%%
# plot the EPV surface
mviz.plot_EPV(
    EPV, field_dimen=(106.0, 68), attack_direction=home_attack_direction
)
#%%
# plot event leading up to first away team goal
mviz.plot_events(
    events.loc[820:823],
    color='k',
    indicators=['Marker', 'Arrow'],
    annotate=True
)
#%%
# Calculate value-added for assist and plot expected value surface
event_id = 822  # away team first goal
#%%

import numpy as np
import pandas as pd
from pandas_profiling import ProfileReport
#%%

pass_frame = events.loc[event_id]['Start Frame']
pass_team = events.loc[event_id].Team
ball_start_pos = np.array(
    [events.loc[event_id]['Start X'], events.loc[event_id]['Start Y']]
)
# print(pass_frame)
frame_home = pd.DataFrame(tracking_home.loc[[pass_frame], ])
frame_away = pd.DataFrame(tracking_away.loc[[pass_frame], ])
pd.concat([frame_home, frame_away], axis=1).T.to_csv('temp2.csv')

#%%
# tracking_home.describe().T.to_csv('tracking_home_describe.csv')
# tracking_away.describe().T.to_csv('tracking_away_describe.csv')
#%%
# profile = ProfileReport(tracking_home, title='home', pool_size = 2, explorative=True)
# profile.to_file('tracking_home_profile.html')
#%%

#%%
event_number = 822
# pull out pass details from the event data
pass_start_pos = np.array(
    [events.loc[event_id]['Start X'], events.loc[event_id]['Start Y']]
)
pass_target_pos = np.array(
    [events.loc[event_id]['End X'], events.loc[event_id]['End Y']]
)
pass_frame = events.loc[event_id]['Start Frame']
pass_team = events.loc[event_id].Team

# direction of play for atacking team (so we know whether to flip the EPV grid)
home_attack_direction = mio.find_playing_direction(tracking_home, 'Home')
if pass_team == 'Home':
    attack_direction = home_attack_direction
    attacking_players = mpc.initialise_players(
        tracking_home.loc[pass_frame], 'Home', params, GK_numbers[0]
    )
    defending_players = mpc.initialise_players(
        tracking_away.loc[pass_frame], 'Away', params, GK_numbers[1]
    )
elif pass_team == 'Away':
    attack_direction = home_attack_direction * -1
    defending_players = mpc.initialise_players(
        tracking_home.loc[pass_frame], 'Home', params, GK_numbers[0]
    )
    attacking_players = mpc.initialise_players(
        tracking_away.loc[pass_frame], 'Away', params, GK_numbers[1]
    )
# flag any players that are offside
attacking_players = mpc.check_offsides(
    attacking_players, defending_players, pass_start_pos, GK_numbers
)
# pitch control grid at pass start location
# Patt_start,_ = mpc.calculate_pitch_control_at_target(pass_start_pos, attacking_players, defending_players, pass_start_pos, params)

#%%
ball_start_pos = pass_start_pos.copy()
target_position = pass_start_pos.copy()
if ball_start_pos is None or any(
    np.isnan(ball_start_pos)
):  # assume that ball is already at location
    ball_travel_time = 0.0
else:
    # ball travel time is distance to target position from current ball position divided assumed average ball speed
    ball_travel_time = np.linalg.norm(target_position - ball_start_pos
                                     ) / params['average_ball_speed']

#%%
# np.linalg.norm(np.array([1.01, 2.01] - np.array([3.01, 4.01])))
# np.linalg.norm(np.array([1.1, 2.1] - np.array([3.1, 4.1])))

#%%
ip = 6 - 1
T = 5
r_final = target_position.copy()
p = attacking_players[ip]
r_reaction = p.position + p.velocity * p.reaction_time
p.time_to_intercept = p.reaction_time + np.linalg.norm(
    r_final - r_reaction
) / p.vmax

den_term = -np.pi / np.sqrt(3.0) / p.tti_sigma * (T - p.time_to_intercept)

den = 1. + np.exp(den_term)
p_intercept = 1 / den
p.time_to_intercept = p_intercept
print(
    f'player_id` = {p.id}, (x, y) = {p.position}, <x, y> = {p.velocity}, `reaction_time = {p.reaction_time}`, r_reaction = {r_reaction}`, `num = {np.linalg.norm(r_final-r_reaction):.3f}`, `tti = {p.time_to_intercept:.2f}`, `den_term = {den_term:.3f}`, `p_intercept` = {p_intercept:.3f}`'
)

#%%
print(p.id)
p.simple_time_to_intercept(target_position)

p.simple_time_to_intercept([-10, -20])
print(p.time_to_intercept)

#%%
[
    f'`player_id` = {p.id}, `tti = {p.simple_time_to_intercept(target_position)}'
    for p in attacking_players
]

#%%
# first get arrival time of 'nearest' attacking player (nearest also dependent on current velocity)
tau_min_att = np.nanmin(
    [p.simple_time_to_intercept(target_position) for p in attacking_players]
)
tau_min_def = np.nanmin(
    [p.simple_time_to_intercept(target_position) for p in defending_players]
)

#%%
# check whether we actually need to solve equation 3
if tau_min_att - max(ball_travel_time,
                     tau_min_def) >= params['time_to_control_def']:
    # if defending team can arrive significantly before attacking team, no need to solve pitch control model
    # return 0., 1.
    res = [0., 1.]

elif tau_min_def - max(ball_travel_time,
                       tau_min_att) >= params['time_to_control_att']:
    # if attacking team can arrive significantly before defending team, no need to solve pitch control model
    # return 1., 0.
    res = [1., 0.]

#%%
# solve pitch control model by integrating equation 3 in Spearman et al.
# first remove any player that is far (in time) from the target location
attacking_players = [
    p for p in attacking_players
    if p.time_to_intercept - tau_min_att < params['time_to_control_att']
]
defending_players = [
    p for p in defending_players
    if p.time_to_intercept - tau_min_def < params['time_to_control_def']
]
# set up integration arrays
dT_array = np.arange(
    ball_travel_time - params['int_dt'],
    ball_travel_time + params['max_int_time'], params['int_dt']
)
PPCFatt = np.zeros_like(dT_array)
PPCFdef = np.zeros_like(dT_array)
# integration equation 3 of Spearman 2018 until convergence or tolerance limit hit (see 'params')
ptot = 0.0
i = 1

#%%
while 1 - ptot > params['model_converge_tol'] and i < dT_array.size:

    T = dT_array[i]
    for player in attacking_players:
        # calculate ball control probablity for 'player' in time interval T+dt
        dPPCFdT = (1 - PPCFatt[i - 1] - PPCFdef[i - 1]
                  ) * player.probability_intercept_ball(T) * player.lambda_att
        # make sure it's greater than zero
        assert dPPCFdT >= 0, 'Invalid attacking player probability (calculate_pitch_control_at_target)'
        if i == 38:
            print(
                f'attacking `ddpcf_dt = {dPPCFdT:.3f}` for `player_id = {player.id}`'
            )
        # print(f'`player_id` = {player.id}, `ppcf` change: {player.PPCF}`, {player.PPCF + dPPCFdT * params["int_dt"]}')
        player.PPCF += dPPCFdT * params['int_dt']

        PPCFatt[i] += player.PPCF
    for player in defending_players:
        # calculate ball control probablity for 'player' in time interval T+dt
        dPPCFdT = (1 - PPCFatt[i - 1] - PPCFdef[i - 1]
                  ) * player.probability_intercept_ball(T) * player.lambda_def
        if i == 38:
            print(
                f'defending `ddpcf_dt = {dPPCFdT:.3f}` for `player_id = {player.id}`'
            )
        # make sure it's greater than zero
        assert dPPCFdT >= 0, 'Invalid defending player probability (calculate_pitch_control_at_target)'
        player.PPCF += dPPCFdT * params[
            'int_dt']  # total contribution from individual player
        PPCFdef[i
               ] += player.PPCF  # add to sum over players in the defending team
    ptot = PPCFdef[i] + PPCFatt[i]  # total pitch control probability
    i += 1

#%%
print(PPCFatt[:40])
print(PPCFdef[:40])
# print(ptot)

#%%
field_dimen = (
    106.,
    68.,
)
# n_grid_cells_x = 50
x, y = pass_start_pos.copy()
if attack_direction == -1:
    EPV = np.fliplr(EPV)
ny, nx = EPV.shape
dx = field_dimen[0] / float(nx)
dy = field_dimen[1] / float(ny)
ix = (x + field_dimen[0] / 2. - 0.0001) / dx
iy = (y + field_dimen[1] / 2. - 0.0001) / dy
EPV[int(iy), int(ix)]
#%%

EPV_start = mepv.get_EPV_at_location(
    pass_start_pos, EPV, attack_direction=attack_direction
)
EPV_target = mepv.get_EPV_at_location(
    pass_target_pos, EPV, attack_direction=attack_direction
)
EPV_start, EPV_target
#%%
PPCF, xgrid, ygrid = mpc.generate_pitch_control_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    params,
    GK_numbers,
    field_dimen=(
        106.,
        68.,
    ),
    n_grid_cells_x=50,
    offsides=True
)
#%%
PPCF
# PPCF[0:4,]
xgrid
ygrid
#%%
fig, ax = mviz.plot_EPV_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    PPCF,
    EPV,
    annotate=True,
    autoscale=True
)
fig.suptitle('Pass EPV added: %1.3f' % EEPV_added, y=0.95)
mviz.plot_pitchcontrol_for_event(
    event_number, events, tracking_home, tracking_away, PPCF, annotate=True
)

#%%
""" **** calculate value-added for all passes **** """

# first get all shots
shots = events[events['Type'] == 'SHOT']
home_shots = shots[shots['Team'] == 'Home']
away_shots = shots[shots['Team'] == 'Away']
# get all  passes
home_passes = events[(events['Type'].isin(['PASS'])) &
                     (events['Team'] == 'Home')]
away_passes = events[(events['Type'].isin(['PASS'])) &
                     (events['Team'] == 'Away')]

# home team value added
home_pass_value_added = []
for i, pass_ in home_passes.iterrows():
    EEPV_added, EPV_diff = mepv.calculate_epv_added(
        i, events, tracking_home, tracking_away, GK_numbers, EPV, params
    )
    home_pass_value_added.append((i, EEPV_added, EPV_diff))

# away team value added
away_pass_value_added = []
for i, pass_ in away_passes.iterrows():
    EEPV_added, EPV_diff = mepv.calculate_epv_added(
        i, events, tracking_home, tracking_away, GK_numbers, EPV, params
    )
    away_pass_value_added.append((i, EEPV_added, EPV_diff))

home_pass_value_added = sorted(
    home_pass_value_added, key=lambda x: x[1], reverse=True
)
away_pass_value_added = sorted(
    away_pass_value_added, key=lambda x: x[1], reverse=True
)

print("Top 5 home team passes by expected EPV-added")
print(home_pass_value_added[:5])
print("Top 5 away team passes by expected EPV-added")
print(away_pass_value_added[:5])

event_number = 1753  # home team assist to header off target
EEPV_added, EPV_diff = mepv.calculate_epv_added(
    event_number, events, tracking_home, tracking_away, GK_numbers, EPV, params
)
PPCF, xgrid, ygrid = mpc.generate_pitch_control_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    params,
    GK_numbers,
    field_dimen=(
        106.,
        68.,
    ),
    n_grid_cells_x=50,
    offsides=True
)
fig, ax = mviz.plot_EPV_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    PPCF,
    EPV,
    annotate=True
)
fig.suptitle('Pass EPV added: %1.3f' % EEPV_added, y=0.95)
mviz.plot_pitchcontrol_for_event(
    event_number, events, tracking_home, tracking_away, PPCF, annotate=True
)

event_number = 1663  # away team assisst to blocked shot
EEPV_added, EPV_diff = mepv.calculate_epv_added(
    event_number, events, tracking_home, tracking_away, GK_numbers, EPV, params
)
PPCF, xgrid, ygrid = mpc.generate_pitch_control_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    params,
    GK_numbers,
    field_dimen=(
        106.,
        68.,
    ),
    n_grid_cells_x=50,
    offsides=True
)
fig, ax = mviz.plot_EPV_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    PPCF,
    EPV,
    annotate=True
)
fig.suptitle('Pass EPV added: %1.3f' % EEPV_added, y=0.95)
mviz.plot_pitchcontrol_for_event(
    event_number, events, tracking_home, tracking_away, PPCF, annotate=True
)

# retaining possession
event_number = 195
EEPV_added, EPV_diff = mepv.calculate_epv_added(
    event_number, events, tracking_home, tracking_away, GK_numbers, EPV, params
)
PPCF, xgrid, ygrid = mpc.generate_pitch_control_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    params,
    GK_numbers,
    field_dimen=(
        106.,
        68.,
    ),
    n_grid_cells_x=50,
    offsides=True
)
fig, ax = mviz.plot_EPV_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    PPCF,
    EPV,
    annotate=True
)
fig.suptitle('Pass EPV added: %1.3f' % EEPV_added, y=0.95)
mviz.plot_pitchcontrol_for_event(
    event_number, events, tracking_home, tracking_away, PPCF, annotate=True
)
'''
 # find maximum possible EPV-added for all home team passes (takes a while to run!)
maximum_EPV_added = []
for i,row in home_passes.iterrows():
    print( 'Event %d' % (i) )
    EEPV_added, EPV_diff = mepv.calculate_epv_added( i, events, tracking_home, tracking_away, GK_numbers, EPV, params)
    max_EEPV_added, target = mepv.find_max_value_added_target( i, events, tracking_home, tracking_away, GK_numbers, EPV, params )
    maximum_EPV_added.append( (i,max_EEPV_added,EEPV_added,EPV_diff))

# sort by the difference between maximum value-added and value-added for the actual pass that was made
# note: some values may be slightly negative because of how the maximum value-added search is performed over a grid
maximum_EPV_added = sorted(maximum_EPV_added,key = lambda x: x[1]-x[2], reverse=True)
'''

# assist example
event_number = 1680
PPCF, xgrid, ygrid = mpc.generate_pitch_control_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    params,
    GK_numbers,
    field_dimen=(
        106.,
        68.,
    ),
    n_grid_cells_x=50,
    offsides=True
)
fig, ax = mviz.plot_EPV_for_event(
    event_number,
    events,
    tracking_home,
    tracking_away,
    PPCF,
    EPV,
    annotate=True,
    autoscale=True,
    contours=True
)

# cross-field passes
examples = [403, 68, 829]
for event_number in examples:
    PPCF, xgrid, ygrid = mpc.generate_pitch_control_for_event(
        event_number,
        events,
        tracking_home,
        tracking_away,
        params,
        GK_numbers,
        field_dimen=(
            106.,
            68.,
        ),
        n_grid_cells_x=50,
        offsides=True
    )
    fig, ax = mviz.plot_EPV_for_event(
        event_number,
        events,
        tracking_home,
        tracking_away,
        PPCF,
        EPV,
        annotate=True,
        autoscale=True,
        contours=True
    )

#%%
