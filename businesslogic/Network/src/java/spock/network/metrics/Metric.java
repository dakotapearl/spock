package spock.network.metrics;

public abstract class Metric<S> {
	
	protected Double minimum = 0.0;
        protected Double maximum = null;
	
        /**
         * 
         * @return 
         */
        public Double getMinimum() {
            return minimum;
        }
        
        /**
         * 
         * @return 
         */
        public Double getMaximum() {
            return maximum;
        }
        
        /**
         * 
         * @param one
         * @param other
         * @return 
         */
        public abstract Double getMetricValue(S one, S other);
        
}
