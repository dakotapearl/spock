package networkDomain.extensions;


/**
 * Instantiated once for whole network
 * @author Loren
 *
 */
public interface EnergyEconomics {

	public void setNXE(NodeExtensionEncapsulator NXE);
	public boolean continueFiring();
	public boolean hasExceededEnergyLimit();
	public double getNodeEfficiency(); // might require notify functions spread through NetworkNode and maybe other extensions
	
}
