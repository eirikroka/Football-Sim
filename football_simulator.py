#!/usr/bin/env python
# coding: utf-8

# # Simulation Football APP

# In[1]:


import pandas as pd  
import numpy as np
#from datetime import datetime, timedelta

import plotly.express as px
from dash.dependencies import Input, Output
from dash import Dash, dcc, html
import dash_bootstrap_components as dbc
from dash_bootstrap_templates import load_figure_template

import dash_cytoscape as cyto

from dash import jupyter_dash
jupyter_dash.default_mode = 'external'


# In[2]:


dbc_css = 'https://cdn.jsdelivr.net/gh/AnnMarieW/dash-bootstrap-templates@V1.0.2/dbc.min.css'


# ## Loading data

# In[3]:


league_results = pd.read_csv('output_for_dash/set_seed/league_results.csv')
el_results = pd.read_csv('output_for_dash/set_seed/el_results.csv')
cl_results = pd.read_csv('output_for_dash/set_seed/cl_results.csv')
ul_results = pd.read_csv('output_for_dash/set_seed/ul_results.csv')

cl_results = cl_results.drop(['Stage'], axis = 1)


# In[4]:


# Pass stands for the number of teams from a given league that enter the champions league
# Europa stands for the number of teams from a given league that enter the europa league
# Elo is the quality of the team gotten from 'http://clubelo.com/'
# Tilt is the average of the sum of goals for and against for a given club (most of this is just self genereated with randomness)
# n_teams is the amount of teams in the league
league_results = league_results.rename(columns = {'seed' : 'Seed'})
league_results.head()


# In[5]:


# Penalty here is a metric over if the knockout match got to penalties
el_results = el_results.rename(columns = {'seed' : 'Seed'})
el_results.head()
el_results[el_results['Round'] == 5]['Winner'].value_counts()


# In[6]:


cols = ['Round'] + [col for col in cl_results.columns if col != 'Round']
cl_results = cl_results[cols]

cl_results = cl_results.rename(columns = {'seed' : 'Seed'})
cl_results.head()
cl_results[cl_results['Round'] == 5]['Winner'].value_counts()
pd.crosstab(cl_results['Winner'], cl_results['Round'])


# In[7]:


ul_results = ul_results.rename(columns = {'seed' : 'Seed'})
ul_results.head()
ul_results[ul_results['Round'] == 5]['Winner'].value_counts()


# ## Functions

# In[8]:


def club_tournament_filter(club, seed, df1 = cl_results, df2 = el_results, df3 = ul_results):
    df1 = df1[df1['Seed'] == seed].copy()
    df2 = df2[df2['Seed'] == seed].copy()
    df3 = df3[df3['Seed'] == seed].copy()
    

    if club in df1['Club_A'].unique() or club in df1['Club_B'].unique():
        df = df1.copy()
        df["Competition"] = "CL"
        
    elif club in df2['Club_A'].unique() or club in df2['Club_B'].unique():
        df = df2.copy()
        df["Competition"] = "EL"

    elif club in df3['Club_A'].unique() or club in df3['Club_B'].unique():
        df = df3.copy()
        df["Competition"] = "UL"
        
    else:
        return pd.DataFrame()

   
    df = df[(df['Club_A'] == club) | (df['Club_B'] == club)]

   
    mask = df['Club_B'] == club

    df.loc[mask, ['Club_A', 'Club_B']] = df.loc[mask, ['Club_B', 'Club_A']].values
    df.loc[mask, ['Result_A', 'Result_B']] = df.loc[mask, ['Result_B', 'Result_A']].values

    
    df = df.drop(columns = [col for col in ['Elo_A', 'Elo_B'] if col in df.columns])

    return df


# In[9]:


club_tournament_filter('Arsenal', 00)


# In[10]:


# Functions
def clean_tournament_table(df):

    subset = df.copy()
    
    subset['Round'] = subset['Round'].replace({
        1: 'R32',
        2: 'R16',
        3: 'QF',
        4: 'SF',
        5: 'F'})

    subset = subset.rename(columns={
        'Club_A': 'Team A',
        'Club_B': 'Team B',
        'Result_A': 'Score A',
        'Result_B': 'Score B',
    }) 

    subset = subset[['Round', 'Team A', 'Score A', 'Team B', 'Score B', 'Penalty']]

    return subset
    

def league_filter(league, seed, df = league_results):
    subset = df[(df['Group'] == league) & (df['Seed'] == seed)].copy()
    subset = subset[['Club', 'Points', 'GF', 'GA', 'GD']]
    
    subset.insert(0, 'Rank', range(1, len(subset) + 1))

    return subset

def club_league_filter(club, seed, df = league_results):
       
    group = df.loc[df['Club'] == club, 'Group'].values[0]

    table = league_filter(group, seed)

    return table
    
def table_fun(df):
    table = html.Div(
        dbc.Table.from_dataframe(
            df,
            striped = True,
            bordered = True,
            hover = True,
            class_name = "table-sm",  
        ),
        style = {
            "overflowX": "auto",
            "width": "100%",
            "fontSize": "14px",  
            "padding": "0px",   
        },
    )

    return(table)


def extract_number_of_teams_in_europa(league, seed, df = league_results):
    
    subset = df[(df['Group'] == league) & (df['Seed'] == seed)].copy()

    n_cl = subset.iloc[0]['Pass']

    n_el = subset.iloc[0]['Europa']

    n_ul = subset.iloc[0]['UL']


    return [n_cl, n_el, n_ul]
    
    
    
    


# In[11]:


def mean_league_table(league, df = league_results):
    subset = df[df['Group'] == league].copy()
    subset = subset[['Club', 'Points', 'GF', 'GA', 'GD']]
    
    avg_stats = subset[['Club', 'Points', 'GF', 'GA', 'GD']].groupby('Club', as_index = False).mean()

    # Round down only numeric columns
    numeric_cols = ['Points', 'GF', 'GA', 'GD']
    avg_stats[numeric_cols] = avg_stats[numeric_cols].apply(np.floor).astype(int)

    avg_table = avg_stats.sort_values(by = ['Points', 'GD', 'GF'], ascending=[False, False, False])

    avg_table.insert(0, 'Rank', range(1, len(avg_table) + 1))
    
    return avg_table

# mean_league_table('NOR')
    


    
    


# In[12]:


def get_tournament_seeds(club, df1=cl_results, df2=el_results, df3=ul_results):
    # Filter rows where the given club is either Club_A or Club_B
    df1 = df1[(df1['Club_A'] == club) | (df1['Club_B'] == club)].copy()
    df2 = df2[(df2['Club_A'] == club) | (df2['Club_B'] == club)].copy()
    df3 = df3[(df3['Club_A'] == club) | (df3['Club_B'] == club)].copy()

    # Initialize seeds as None
    seed1 = seed2 = seed3 = None

    # Check if each dataframe is non-empty before proceeding
    if not df1.empty:
        max_round = df1['Round'].max()

        if ((df1['Round'] == 5) & (df1['Winner'] == club)).any():
            seed1 = df1[(df1['Round'] == 5) & (df1['Winner'] == club)]['Seed'].unique()
        else:
            seed1 = df1[df1['Round'] == max_round]['Seed'].unique()
    else:
        df1 = None

    if not df2.empty:
        max_round = df2['Round'].max()
    
        if ((df2['Round'] == 5) & (df2['Winner'] == club)).any():
            seed2 = df2[(df2['Round'] == 5) & (df2['Winner'] == club)]['Seed'].unique()
        else:
            seed2 = df2[df2['Round'] == max_round]['Seed'].unique()
    else:
        df2 = None

    if not df3.empty:
        max_round = df3['Round'].max()
    
        if ((df3['Round'] == 5) & (df3['Winner'] == club)).any():
            seed3 = df3[(df3['Round'] == 5) & (df3['Winner'] == club)]['Seed'].unique()
        else:
            seed3 = df3[df3['Round'] == max_round]['Seed'].unique()
    else:
        df3 = None

 
    return [seed1, seed2, seed3]
    
  
    

get_tournament_seeds('Nantes')


# In[ ]:





# In[13]:


def best_tournament_result(club, df1=cl_results, df2=el_results, df3=ul_results):
    # Filter rows where the given club is either Club_A or Club_B
    df1 = df1[(df1['Club_A'] == club) | (df1['Club_B'] == club)].copy()
    df2 = df2[(df2['Club_A'] == club) | (df2['Club_B'] == club)].copy()
    df3 = df3[(df3['Club_A'] == club) | (df3['Club_B'] == club)].copy()

    # Initialize seeds as None
    seed1 = seed2 = seed3 = None

    # Check if each dataframe is non-empty before proceeding
    if not df1.empty:
        seed1 = df1.loc[df1['Round'].idxmax(), 'Seed']
        df1 = df1[df1['Seed'] == seed1].copy()
    else:
        df1 = None  # If empty, set to None

    if not df2.empty:
        seed2 = df2.loc[df2['Round'].idxmax(), 'Seed']
        df2 = df2[df2['Seed'] == seed2].copy()
    else:
        df2 = None  # If empty, set to None

    if not df3.empty:
        seed3 = df3.loc[df3['Round'].idxmax(), 'Seed']
        df3 = df3[df3['Seed'] == seed3].copy()
    else:
        df3 = None  # If empty, set to None

    # Return the filtered dataframes (or None for empty datasets)
    return [df1, df2, df3]
    
  
    


    


# In[14]:


best_tournament_result('Aston Villa')


# In[15]:


# This function is very much ChatGpt generated, it makes the layout for a cup tree

def generate_bracket_elements(df):
    elements = []
    rounds = sorted(df['Round'].unique(), reverse=True)
    all_matches_by_round = {r: df[df['Round'] == r].copy().reset_index(drop=True) for r in rounds}

    ordered_matches = {rounds[0]: all_matches_by_round[rounds[0]]}  # Start with final

    # Build the full match order backwards
    for i in range(1, len(rounds)):
        r_now = rounds[i - 1]     # e.g. round 5
        r_prev = rounds[i]        # e.g. round 4

        matches_now = ordered_matches[r_now]
        matches_prev = all_matches_by_round[r_prev]

        new_order = []
        used = set()

        for _, match in matches_now.iterrows():
            a = match['Club_A']
            b = match['Club_B']

            for club in [a, b]:
                found = False
                for idx, prev_match in matches_prev.iterrows():
                    if idx in used:
                        continue
                    if prev_match['Winner'] == club:
                        new_order.append(prev_match)
                        used.add(idx)
                        found = True
                        break
                if not found:
                    # fallback: add empty Series if not found
                    new_order.append(pd.Series(dtype=object))

        # Fill missing matches if needed (walkovers, etc.)
        remaining = matches_prev[~matches_prev.index.isin(used)]
        for _, row in remaining.iterrows():
            new_order.append(row)

        ordered_matches[r_prev] = pd.DataFrame(new_order).reset_index(drop=True)

    # Reverse the ordered dict back to ascending round order
    final_ordered = {r: ordered_matches[r] for r in sorted(ordered_matches)}

    positions = {}         # Map (team, round) -> match_id
    match_positions = {}   # Map match_id -> {'x': x, 'y': y}

    x_spacing = 400
    min_round = min(final_ordered.keys())

    for round_num, matches in final_ordered.items():
        x = round_num * x_spacing

        if round_num == min_round:
            # Base round: assign initial y positions evenly spaced
            y_spacing = 80
            num_matches = len(matches)
            total_y = (num_matches - 1) * y_spacing
            start_y = total_y / -2  # center around y=0

            for i, row in matches.iterrows():
                if isinstance(row, pd.Series) and row.empty:
                    continue

                y = start_y + i * y_spacing
                match_id = f"{row['Club_A']}_{row['Club_B']}_R{row['Round']}"

                # Save position
                match_positions[match_id] = {'x': x, 'y': y}

                try:
                    score_a = int(row['Result_A'])
                    score_b = int(row['Result_B'])
                except:
                    score_a = score_b = 0

                winner = row.get('Winner', '')
                if winner == row['Club_A']:
                    label = f"*{row['Club_A']}* {score_a} - {score_b} {row['Club_B']}"
                elif winner == row['Club_B']:
                    label = f"{row['Club_A']} {score_a} - {score_b} *{row['Club_B']}*"
                else:
                    label = f"{row['Club_A']} {score_a} - {score_b} {row['Club_B']}"

                #label = f"{row['Club_A']} {score_a} - {score_b} {row['Club_B']}"

                elements.append({
                    'data': {'id': match_id, 'label': label},
                    'position': {'x': x, 'y': y},
                    'classes': 'match'
                })

                positions[(row['Club_A'], round_num)] = match_id
                positions[(row['Club_B'], round_num)] = match_id

        else:
            # Later rounds: position matches at average y of predecessor matches
            for i, row in matches.iterrows():
                if isinstance(row, pd.Series) and row.empty:
                    continue

                match_id = f"{row['Club_A']}_{row['Club_B']}_R{row['Round']}"
                prev_round = round_num - 1

                # Find the source matches for the two teams
                source_ids = []
                for team in [row['Club_A'], row['Club_B']]:
                    source_id = positions.get((team, prev_round))
                    if source_id:
                        source_ids.append(source_id)

                # Compute average y position of source matches
                if len(source_ids) == 2:
                    y = (match_positions[source_ids[0]]['y'] + match_positions[source_ids[1]]['y']) / 2
                elif len(source_ids) == 1:
                    y = match_positions[source_ids[0]]['y']
                else:
                    y = 0  # fallback default

                # Save position
                match_positions[match_id] = {'x': x, 'y': y}

                try:
                    score_a = int(row['Result_A'])
                    score_b = int(row['Result_B'])
                except:
                    score_a = score_b = 0

                winner = row.get('Winner', '')
                if winner == row['Club_A']:
                    label = f"*{row['Club_A']}* {score_a} - {score_b} {row['Club_B']}"
                elif winner == row['Club_B']:
                    label = f"{row['Club_A']} {score_a} - {score_b} *{row['Club_B']}*"
                else:
                    label = f"{row['Club_A']} {score_a} - {score_b} {row['Club_B']}"

                elements.append({
                    'data': {'id': match_id, 'label': label},
                    'position': {'x': x, 'y': y},
                    'classes': 'match',
                    'locked': True,
                })

                positions[(row['Club_A'], round_num)] = match_id
                positions[(row['Club_B'], round_num)] = match_id

                # Add edges from previous matches to this match
                for source_id in source_ids:
                    elements.append({
                        'data': {'source': source_id, 'target': match_id}
                    })

    return elements


# ## APP - pre work

# In[ ]:





# In[16]:


# Options
# options_club = [
#     {'label': name, 'value': name} for name in league_results['Club'].unique()
# ]

# options_league = [
#     {'label': name, 'value': name} for name in league_results['Group'].unique()
# ]

options_league = [
    {'label': 'Champions League', 'value': 'cl'},
    {'label': 'Europa League', 'value': 'el'},
    {'label': 'Conference League', 'value': 'ul'},
    {'label': 'England', 'value': 'ENG'},
    {'label': 'Spain', 'value': 'ESP'},
    {'label': 'Germany', 'value': 'GER'},
    {'label': 'Italy', 'value': 'ITA'},
    {'label': 'France', 'value': 'FRA'},
    {'label': 'Austria', 'value': 'AUT'},
    {'label': 'Belgium', 'value': 'BEL'},
    {'label': 'Croatia', 'value': 'CRO'},
    {'label': 'Czechia', 'value': 'CZE'},
    {'label': 'Denmark', 'value': 'DEN'},
    {'label': 'Greece', 'value': 'GRE'},
    {'label': 'Hungary', 'value': 'HUN'},
    {'label': 'Israel', 'value': 'ISR'},
    {'label': 'Netherlands', 'value': 'NED'},
    {'label': 'Norway', 'value': 'NOR'},
    {'label': 'Poland', 'value': 'POL'},
    {'label': 'Portugal', 'value': 'POR'},
    {'label': 'Rest of Europe', 'value': 'REST'},
    {'label': 'Romania', 'value': 'ROM'},
    {'label': 'Russia', 'value': 'RUS'},
    {'label': 'Scotland', 'value': 'SCO'},
    {'label': 'Serbia', 'value': 'SRB'},
    {'label': 'Switzerland', 'value': 'SUI'},
    {'label': 'Slovenia', 'value': 'SVN'},
    {'label': 'Sweden', 'value': 'SWE'},
    {'label': 'Turkiye', 'value': 'TUR'},
    {'label': 'Ukraine', 'value': 'UKR'},
]


# In[17]:


len(league_results['Seed'].unique())


# In[18]:


# Input
league_selector = dcc.Dropdown(
    id = 'my_league',
    options = options_league,
    value = 'cl',
    clearable = False
)

# For filtering for club
league_selector_filter = dcc.Dropdown(
    id = 'my_league_filter',
    options = [opt for opt in options_league if opt['value'].lower() not in ['cl', 'el', 'ul']],
    value = 'ENG',
    clearable = False
)

# Dependent on the league selector filter
club_selector = dcc.Dropdown(
    id = 'my_club',
    options = None,
    value = 'Chelsea',
    clearable = False
)

seed_selector = dcc.Dropdown(
    id = 'my_seed',
    #options = [{'label': str(i), 'value': i} for i in range(1, len(league_results['Seed'].unique()) + 1)],
    options = [{'label': str(i), 'value': i} for i in sorted(league_results['Seed'].unique())],
    value = 1,  # default value
    clearable = False,
    style = {'width': '100px'}  # optional: control the size
)

# Output
info_league = html.Div(
    id = 'info_league'
)

league_table1 = html.Div(
    id = 'league_table1',
)

average_league_table = html.Div(
    id = 'average_league_table',
)

info_club = html.Div(
    id = 'info_club'
)

league_table2 = html.Div(
    id = 'league_table2',
)

europa_tournament = html.Div(
    id = 'europa_tournament'
)


# In[19]:


# Cyto for cup tree
# Made by chat gpt
cyto = cyto.Cytoscape(
        id = 'cyto',
        elements= [],
        style={'width': '100%', 'height': '900px', 'border': '1px solid black',
             #  'display': 'none' 
              },
        layout={
            'name': 'preset',
            'zoom': 0.45,
            'pan': {'x': -75, 'y': 475}
        },
        userZoomingEnabled = False,      # Disable zooming
        userPanningEnabled = False,      # Disable panning
        boxSelectionEnabled = False,     # Disable box selection
        autoungrabify = True,
        stylesheet=[
            {
                'selector': 'node',
                'style': {
                    'content': 'data(label)',
                    'text-valign': 'center',
                    'text-halign': 'center',
                    'background-color': 'green',
                    'color': 'white',
                    'width': 350,
                    'height': 40,
                    'font-size': 20,
                    'shape': 'roundrectangle',
                    'padding': '10px'
                }
            },
            {
                'selector': 'edge',
                'style': {
                    'target-arrow-shape': 'triangle',
                    'curve-style': 'bezier',
                    'arrow-scale': 2,
                    'line-color': '#aaa',
                    'target-arrow-color': '#aaa'
                }
            }
        ]
    )


# In[20]:


# Tabs

tab_league = dcc.Tab(
    label = 'League',
    children = [
        html.Br(),
        league_selector,
        html.Br(),
        info_league,
        html.Br(),
        league_table1,
        html.Br(),
        cyto,
        html.Br(),
        html.P('The average league across all seeds:'), # Remove this later
        average_league_table
    ]
)

tab_club = dcc.Tab(
    label = 'Club',
    children = [
        html.Br(),
        league_selector_filter,
        html.Br(),
        club_selector,
        html.Br(),
        html.Br(),
        info_club,
        html.Br(),
        dbc.Row(
            dbc.Col(
                dbc.Card([
                    html.Br(),
                    europa_tournament],
                        style = {'textAlign' : 'center'}),
                width = 8
            ),
            justify = 'center'
        ),
        html.Br(),
        html.Br(),
        dbc.Row(
            dbc.Col(
                dbc.Card([
                    html.Br(),
                    html.H4('League table:'),
                    league_table2
                ], 
                         style = {'textAlign' : 'center'}
                        ),
                width = 10
            ),
            justify = 'center'
        ),

    ]
)

tabs = dbc.Row(
        dbc.Col(
            dcc.Tabs([
                tab_league,
                tab_club
            ]),
            #width = 8  # or adjust as needed
        ),
      #  justify='center'
    )



# ## The APP

# In[21]:


app = Dash(__name__ ,external_stylesheets = [dbc.themes.LUX, dbc_css])
server = app.server

app.layout = dbc.Container(
    children = [

        html.Br(),

        html.H1('Eiriks football simulations results overview', style = {'textAlign' : 'center'}),

        html.Br(),

        html.Div([
            html.P('Please select a seed: '),
            seed_selector,
            ]),

        html.Br(),

        dbc.Card(tabs),
        
        html.Br(),

        
        
    ],
    className = 'dbc'
)


# In[ ]:





# In[22]:


# Tab league ----------------------------------------------------------------------------------------
@app.callback(
    Output('league_table1', 'children'),
    Output('info_league', 'children'),
    Output('cyto', 'elements'),
    Output('cyto', 'style'),
    Output('cyto', 'layout'), 
    Input('my_league', 'value'),
    Input('my_seed', 'value'),
)
def update_league(league, seed, df1 = cl_results, df2 = el_results, df3 = ul_results):
    if league in ['cl', 'el', 'ul']:

        if league == 'cl':
            subset = df1.copy()
        elif league == 'el': 
            subset = df2.copy()
        else:
            subset = df3.copy()

        subset = subset[subset['Seed'] == seed]
        cyto_elements = generate_bracket_elements(subset)  
        
        return None, None, cyto_elements,  {
            'display': 'flex',
            'width': '100%',
            'height': '900px',
            'border': '1px solid black'
        }, {'name': 'preset'} 

    else:
     
        subset = league_filter(league, seed)
        
        table = table_fun(subset)

        europe_lst = extract_number_of_teams_in_europa(league, seed)

        info = dbc.Row(
            dbc.Col(
                html.Ul(
                    [
                        html.Li(f'Number of teams in Champions League: {europe_lst[0]}'),
                        html.Li(f'Number of teams in Europa League: {europe_lst[1]}'),
                        html.Li(f'Number of teams in Conference League: {europe_lst[2]}'),
                        
                    ],
                    style={
                        'textAlign': 'center',
                        'listStylePosition': 'inside',
                        'paddingLeft': 0
                    }
                )
            )
        )
                        
        
        return table, info, [], {"display": "none"} , {'name': 'preset'} 

@app.callback(
    Output(component_id = 'average_league_table', component_property = 'children'),
    Input(component_id = 'my_league', component_property = 'value'), 
)

def update_average_league(league):
    if league in ['cl', 'el', 'ul']:
        return ' '
    
    else:
        subset = mean_league_table(league)
        table = table_fun(subset)

    return table
    
# Tab club ----------------------------------------------------------------------------------------
@app.callback(
    Output(component_id = 'my_club', component_property = 'options'),
    Input(component_id = 'my_league_filter', component_property = 'value'), 
)

def update_options(league, df = league_results):
    subset = df[df['Group'] == league].copy()

    options_club = [
        {'label': name, 'value': name} 
        for name in sorted(subset['Club'].unique())
    ]

    return options_club


@app.callback(
    Output(component_id = 'league_table2', component_property = 'children'),
    Input(component_id = 'my_league_filter', component_property = 'value'), 
    Input('my_seed', 'value'),
)

def update_league2(league, seed):
    
    subset = league_filter(league, seed)
    table = table_fun(subset)

    return table



@app.callback(
    Output(component_id = 'info_club', component_property = 'children'),
    Input(component_id = 'my_club', component_property = 'value'), 
)

def update_info_club(club):
    seeds = get_tournament_seeds(club)

    cl = seeds[0]

    el = seeds[1]

    ul = seeds[2]

    children = html.Div([
        dbc.Row(
            html.P([
                html.Span(f"{club}'s", style={'fontWeight': 'bold', 'color': 'blue'}),
                f' best Champions league seed(s): {cl}'
            ])
        ),
        dbc.Row(
            html.P([
                html.Span(f"{club}'s", style={'fontWeight': 'bold', 'color': 'blue'}),
                f' best Europa league seed(s): {el}'
            ])
        ),
        dbc.Row(
            html.P([
                html.Span(f"{club}'s", style={'fontWeight': 'bold', 'color': 'blue'}),
                f' best Conference league seed(s): {ul}'
            ])
        )
    ])

    return children


@app.callback(
    Output(component_id = 'europa_tournament', component_property = 'children'),
    Input(component_id = 'my_club', component_property = 'value'), 
    Input('my_seed', 'value'),
)

def update_europa_tournament(club, seed):
    
    subset = club_tournament_filter(club, seed)

    if subset.empty:
        return html.P('Europe: This club did not play in Europe')

    if 'CL' in subset['Competition'].unique():
        title = html.H4('Champions League:', style = {'color' : 'blue'})

    if 'EL' in subset['Competition'].unique():
        title = html.H4('Europa League:', style = {'color' : 'orange'})

    if 'UL' in subset['Competition'].unique():
        title = html.H4('Conference League:', style = {'color' : 'green'})

    subset = clean_tournament_table(subset)
      
    table = table_fun(subset)

    children = html.Div([
        title,
        table
    ])

    return children
    
    

if __name__ == '__main__':
    app.run(debug = True)



