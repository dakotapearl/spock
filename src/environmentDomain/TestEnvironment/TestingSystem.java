package environmentDomain.TestEnvironment;

import environmentDomain.SensoryMotorSystem;

public class TestingSystem extends SensoryMotorSystem {

	@Override
	public void initialise() {
		addAction(new Action_Test());
		addPerception(new Perception_Test(environmentDomain.dataDomain));
	}
	
}
