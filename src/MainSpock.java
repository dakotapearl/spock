import tools.Log;
import dataDomain.DataDomain;
import environmentsDomain.*;
import experimentsDomain.ExperimentsDomain;
import networkDomain.*;
import mechanismsDomain.*;

public class NeuralNetwork extends Application {
	
	public void initialise() {
		
		Log.write("Application (1): making domains");
		ExperimentsDomain experimentsDomain  = new ExperimentsDomain();
		EnvironmentsDomain environmentsDomain  = new EnvironmentsDomain();
		NetworkDomain networkDomain  = new NetworkDomain();
		DataDomain dataDomain = new DataDomain();
		
		Log.write("Application (2): connecting domains");
		experimentsDomain.setEnvironmentEE(environmentsDomain);
		experimentsDomain.setNetworkEE(networkDomain);
		environmentsDomain.setExperimentsEE(experimentsDomain);
		environmentsDomain.setDataEE(dataDomain);
		networkDomain.setExperimentsEE(experimentsDomain);
		networkDomain.setDataEE(dataDomain);
		
		Log.write("Application (3): selecting experiment");
		experimentsDomain.setExperiment(ExperimentsDomain.XID_TEXTUAL_ONE);
		
		Log.write("Application (4): initialising domains");
		environmentsDomain.initialise();
		networkDomain.initialise();
		experimentsDomain.initialise();
		
		Log.write("Application (5): starting application queue");
		ApplicationEventQueue.start();
		
	}
	
	public static void main(String[] args) {

		Log.LoggingEnabled = true;
		Log.CreationLogsEnabled = false;
		Log.MechanismDebugEnabled = false;
		Log.TimeStampEnabled = true;
		
		new NeuralNetwork().initialise();
		
	}
}
