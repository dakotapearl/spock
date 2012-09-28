package environmentDomain.TestEnvironment;

import environmentDomain.EnvironmentDomain;
import environmentDomain.types.SensoryMotorSystem;

/**
 * @author Loren Chorley
 */
public class TestingSystem extends SensoryMotorSystem {

	public TestingSystem(EnvironmentDomain environmentDomain) {
		super(environmentDomain);
		actions.add(new Action_Test());
		perceptions.add(new Perception_Test(environmentDomain));
	}

	@Override
	public void startEnvironment() {
		
	}
	
}
