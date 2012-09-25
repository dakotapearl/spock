package goalDomain.flavours.implementations;

import java.util.Date;
import static java.lang.Math.*;
import goalDomain.flavours.SpikeAndDecayGoal;

public class ExponentialDecayGoal extends SpikeAndDecayGoal {

	//Decay passes through the point:
	private final double timePoint = 2000; //ms
	private final double rewardPoint = 0.05; //percent of lastReward
	
	@Override
	public double getReward() {
		double elapsed = (new Date()).getTime() - lastTime.getTime();
		double coefficient = log(rewardPoint / lastReward) / timePoint;
		
		return lastReward * exp(coefficient * elapsed);
	}

	@Override
	public void start() {
		
	}

}
