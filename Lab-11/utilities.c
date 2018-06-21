#include "mdp.h"
#include "utilities.h"

#include <stdio.h> // for error message only

double
calc_eu ( const mdp *  p_mdp, unsigned int state, const double * utilities,
          const unsigned int action)
{

  if(p_mdp->numAvailableActions[state] == 0)
    return 0.0;
  
  double eu = 0.0;   // Expected utility

  unsigned int new_state;

  for(new_state = 0; new_state < p_mdp->numStates; new_state++)
    eu += p_mdp->transitionProb[new_state][state][action]
      * utilities[new_state];
    
  return eu;
}

void
calc_meu ( const mdp * p_mdp, unsigned int state, const double * utilities,
           double * meu, unsigned int * action )
{
  if(p_mdp->numAvailableActions[state] == 0)
    return;
  
  unsigned int act;
  double max_eu = calc_eu(p_mdp, state, utilities, p_mdp->actions[state][0]);
  double temp = 0.0;
  unsigned int max_action = 0;

  for(act = 1; act < p_mdp->numAvailableActions[state]; act++)
    {
      temp = calc_eu(p_mdp, state, utilities, p_mdp->actions[state][act]);
      
      if(temp > max_eu)
        {
          max_eu = temp;
          max_action = p_mdp->actions[state][act];
        }
    }
  *meu = max_eu;
  *action = max_action;
}
