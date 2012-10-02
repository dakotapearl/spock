package metricDomain.goals.flavours;


import java.util.Date;

import metricDomain.goals.Goal;

/**
 * @author Loren Chorley
 */
public abstract class SpikeAndDecayGoal implements Goal {

	protected double lastReward;
	protected Date lastTime;
	
    public void spike(double reward) {
    	lastTime = new Date();
    	lastReward = reward + getReward();
    }
    
    public abstract double getReward(); /*computes reward based on lastReward and lastTime*/
	
}