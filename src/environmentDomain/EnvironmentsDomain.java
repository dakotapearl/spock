package environmentsDomain;

import tools.Assert;
import tools.Log;
import dataDomain.DataClientInterface;
import dataDomain.DataDomain;
import experimentsDomain.ExperimentsDomain;
import experimentsDomain.ExperimentsToEnvironmentsClientInterface;
import experimentsDomain.ExperimentsToEnvironmentsServerInterface;
import mechanismsDomain.Domain;

public class EnvironmentsDomain extends Domain implements ExperimentsToEnvironmentsServerInterface {

	private ExperimentsToEnvironmentsClientInterface experimentsEE;
	private DataClientInterface dataEE;
	
	public void initialise() {
		Log.write("Environments domain initialised");
	}
	
	public void setExperimentsEE(ExperimentsToEnvironmentsClientInterface experimentsEE) {
		this.experimentsEE = experimentsEE;
	}
	
	public ExperimentsDomain getExperimentsDomain() {
		return (ExperimentsDomain) experimentsEE;
	}
	
	public void setDataEE(DataClientInterface dataEE) {
		this.dataEE = dataEE;
	}
	
	public DataDomain getDataDomain() {
		return (DataDomain) dataEE;
	}
	
	@SuppressWarnings("unchecked")
	public SensoryMotorSystem newSensoryMotorSystem(String qualifiedClassName) {
		if (!qualifiedClassName.startsWith("environmentsDomain."))
			qualifiedClassName = "environmentsDomain." + qualifiedClassName;
		
		try {
			
			Class c = Class.forName(qualifiedClassName);
			Object o = c.newInstance();
			
			Assert.CriticalAssertTrue("Specified instance is a SensoryMotorSystem", o instanceof SensoryMotorSystem);
			
			SensoryMotorSystem env = (SensoryMotorSystem) o;
			
			env.setEnvironmentsDomain(this);
			env.initialise();
			
			return env;
			
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (InstantiationException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}
	
}
