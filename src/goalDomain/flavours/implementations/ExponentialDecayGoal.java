package goalDomain.flavours.implementations;

import java.util.Date;

import tools.errorChecking.Assert;
import static java.lang.Math.*;
import goalDomain.flavours.SpikeAndDecayGoal;

public class ExponentialDecayGoal extends SpikeAndDecayGoal {

	//Decay passes through the point:
	private double timePoint;
	private double rewardPoint;
	
	public ExponentialDecayGoal() {
		timePoint = 2000; //ms
		rewardPoint = 0.05; //percent of lastReward
	}
	
	public ExponentialDecayGoal(double timePoint, double rewardPoint) {
		Assert.CriticalAssertTrue("ExponentialDecayGoal: timePoint > 0", timePoint > 0);
		Assert.CriticalAssertTrue("ExponentialDecayGoal: rewardPoint > 0", rewardPoint > 0);
		
		this.timePoint = timePoint;
		this.rewardPoint = rewardPoint;
	}
	
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
